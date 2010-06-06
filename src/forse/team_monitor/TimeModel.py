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
from Util import secondsToString


class TimeModel(QAbstractTableModel):

    def __init__(self, carId):
        QAbstractTableModel.__init__(self)
        self.__id = carId
        self.__best = {}
        self.__last = {}
        self.__numInterm = 0
        handlers = {('init', 'best_lap'): self._newBestLap,
                    ('init', 'best_time'): self._newBestTime,
                    ('init', 'chrono'): self._newChrono,
                    ('init', 'last_lap'): self._newLastLap}
        OTPApplication.registerMsgHandlers(handlers)

    def columnCount(self, _parent):
        return 2

    def rowCount(self, _parent):
        return max(len(self.__best), len(self.__last))

    def data(self, index, role):
        if role == Qt.DisplayRole:
            i = 'lap' if index.row() == self.__numInterm else index.row() + 1
            try:
                if index.column() == 0:
                    return QVariant(self.__last[i])
                elif index.column() == 1:
                    return QVariant(self.__best[i])
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
            elif section == self.__numInterm:
                return QVariant("Lap")
            else:
                return QVariant("I%i" % (section + 1))
        elif role == Qt.TextAlignmentRole:
            if orientation == Qt.Vertical:
                return QVariant(Qt.AlignCenter)
        return QVariant()

    def _newBestLap(self, car, time):
        if car == self.__id:
            self.__best['lap'] = secondsToString(time)
            self.reset()

    def _newBestTime(self, car, intermediate, time):
        if car == self.__id:
            self.__best[intermediate] = secondsToString(time)
            self.__numInterm = max(self.__numInterm, intermediate)
            self.reset()

    def _newChrono(self, car, intermediate, _lap, time, _speed):
        if car == self.__id:
            self.__last[intermediate] = secondsToString(time)
            self.__numInterm = max(self.__numInterm, intermediate)
            self.reset()

    def _newLastLap(self, car, time):
        if car == self.__id:
            self.__last['lap'] = secondsToString(time)
            self.reset()
