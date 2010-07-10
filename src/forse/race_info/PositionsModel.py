###########################################################################
#
# Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
#
# This file is part of FORSE.
#
# FORSE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# FORSE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
#
###########################################################################

from PyQt4.Qt import Qt
from PyQt4.QtCore import QAbstractTableModel, QVariant
from PyQt4.QtGui import QIcon
from OTPApplication import OTPApplication
from PilotInfo import PilotInfo


class PositionsModel(QAbstractTableModel):

    def __init__(self):
        QAbstractTableModel.__init__(self)
        self.__arrowDown = QIcon(":/icons/arrow-down.png")
        self.__arrowUp = QIcon(":/icons/arrow-up.png")
        self.__positions = {}
        handlers = {('init', 'standings'): self._standingsInit,
                    ('update', 'standings'): self._standingsUpdate}
        OTPApplication.registerMsgHandlers(handlers)

    def columnCount(self, _parent):
        return 3

    def rowCount(self, _parent):
        return len(self.__positions)

    def data(self, index, role):
        if role == Qt.DisplayRole:
            try:
                i = self.__positions[index.row() + 1]
                if index.column() == 0:
                    return QVariant(i)
                elif index.column() == 1:
                    return QVariant(PilotInfo.get(i).name())
                elif index.column() == 2:
                    return QVariant(PilotInfo.get(i).state())
            except KeyError:
                pass
        elif role == Qt.TextAlignmentRole:
            if index.column() == 0 or index.column() == 2:
                return QVariant(Qt.AlignCenter)
        return QVariant()

    def headerData(self, section, orientation, role):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                if section == 0:
                    return QVariant("ID")
                elif section == 1:
                    return QVariant("Pilot")
                elif section == 2:
                    return QVariant("State")
            else:
                return QVariant(section + 1)
        elif role == Qt.DecorationRole:
            if orientation == Qt.Vertical:
                try:
                    return QVariant(PilotInfo.get(self.__positions[section + 1]).icon())
                except KeyError:
                    pass
        elif role == Qt.TextAlignmentRole:
            if orientation == Qt.Vertical:
                return QVariant(Qt.AlignCenter)
        return QVariant()

    def _standingsInit(self, standings):
        self.__positions = dict([(pos, pilot) for pilot, pos in standings])
        self.reset()

    def _standingsUpdate(self, standings):
        for pilot, newpos in standings:
            for oldpos, oldpilot in self.__positions.iteritems():
                if oldpilot == pilot:
                    if newpos > oldpos:
                        PilotInfo.get(pilot).setIcon(self.__arrowDown)
                    elif newpos < oldpos:
                        PilotInfo.get(pilot).setIcon(self.__arrowUp)
                    break
        for pilot, newpos in standings:
            self.__positions[newpos] = pilot
        self.reset()
