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
from OTPApplication import OTPApplication
from Util import mpsToString


class SpeedModel(QAbstractTableModel):

    def __init__(self, carId):
        QAbstractTableModel.__init__(self)
        self.__id = carId
        self.__best = {}
        self.__last = {}
        handlers = {('init', 'best_speed'): self._newBestSpeed,
                    ('init', 'chrono'): self._newChrono}
        OTPApplication.registerMsgHandlers(handlers)

    def columnCount(self, _parent):
        return 2

    def rowCount(self, _parent):
        return max(len(self.__best), len(self.__last))

    def data(self, index, role):
        if role == Qt.DisplayRole:
            try:
                if index.column() == 0:
                    return QVariant(self.__last[index.row() + 1])
                elif index.column() == 1:
                    return QVariant(self.__best[index.row() + 1])
            except KeyError:
                return QVariant("N/A")
        elif role == Qt.TextAlignmentRole:
            return QVariant(Qt.AlignRight | Qt.AlignVCenter)
        return QVariant()

    def headerData(self, section, orientation, role):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                if section == 0:
                    return QVariant("Last")
                elif section == 1:
                    return QVariant("Best")
            else:
                return QVariant("I%i" % (section + 1))
        elif role == Qt.TextAlignmentRole:
            if orientation == Qt.Vertical:
                return QVariant(Qt.AlignCenter)
        return QVariant()

    def _newBestSpeed(self, car, intermediate, speed):
        if car == self.__id:
            self.__best[intermediate] = mpsToString(speed)
            self.reset()

    def _newChrono(self, car, intermediate, _lap, _time, speed):
        if car == self.__id:
            self.__last[intermediate] = mpsToString(speed)
            self.reset()
