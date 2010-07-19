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
from PyQt4.QtCore import QAbstractTableModel, QVariant, pyqtSignal
from OTPApplication import OTPApplication
from PilotInfo import PilotInfo
from Util import secondsToString


class TelemetryModel(QAbstractTableModel):

    newIntermediate = pyqtSignal(int, int)
    newLap = pyqtSignal(int)

    def __init__(self):
        QAbstractTableModel.__init__(self)
        self.__data = {}
        self.__intermediate = None
        self.__lap = None
        handlers = {('init', 'chrono'): self._chronoInit,
                    ('update', 'chrono'): self._chronoUpdate}
        OTPApplication.registerMsgHandlers(handlers)

    def columnCount(self, _parent):
        return 3

    def rowCount(self, _parent):
        try:
            return len(self.__data[self.__lap][self.__intermediate])
        except KeyError:
            return 0

    def data(self, index, role):
        if role == Qt.DisplayRole:
            try:
                t = self.__data[self.__lap][self.__intermediate][index.row()]
                if index.column() == 0:
                    return QVariant(PilotInfo.get(t[0]).name())
                elif index.column() == 1:
                    return QVariant(PilotInfo.get(t[0]).teamName())
                elif index.column() == 2:
                    if index.row() == 0:
                        return QVariant("%s (%s)" % (secondsToString(t[1]),
                                                     secondsToString(t[2])))
                    else:
                        return QVariant("+ " + secondsToString(t[1]))
            except KeyError:
                pass
        elif role == Qt.TextAlignmentRole:
            if index.column() == 2:
                return QVariant(Qt.AlignRight | Qt.AlignVCenter)
        return QVariant()

    def headerData(self, section, orientation, role):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                if section == 0:
                    return QVariant("Pilot")
                elif section == 1:
                    return QVariant("Team")
                elif section == 2:
                    return QVariant("Time")
            else:
                return QVariant(section + 1)
        elif role == Qt.TextAlignmentRole:
            if orientation == Qt.Vertical:
                return QVariant(Qt.AlignCenter)
        return QVariant()

    def setLapAndIntermediate(self, lap, intermediate):
        self.__lap = lap
        self.__intermediate = intermediate
        self.reset()

    def _chronoInit(self, car, intermediate, lap, reltime, time):
        newLap = False
        if lap not in self.__data:
            newLap = True
            self.__data[lap] = {}
        self.__data[lap][intermediate] = [(car, reltime, time)]
        if newLap:
            self.newLap.emit(lap)
        self.newIntermediate.emit(lap, intermediate)

    def _chronoUpdate(self, car, intermediate, lap, time):
        try:
            delta = time - self.__data[lap][intermediate][0][2]
            self.__data[lap][intermediate].append((car, delta))
            if lap == self.__lap and intermediate == self.__intermediate:
                self.reset()
        except KeyError:
            pass
