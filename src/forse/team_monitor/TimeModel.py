from PyQt4.Qt import Qt
from PyQt4.QtCore import QAbstractTableModel, QVariant
from OTPApplication import OTPApplication
from Util import secondsToString

# TODO: calculate and show last lap time

class TimeModel(QAbstractTableModel):

    def __init__(self, carId):
        QAbstractTableModel.__init__(self)
        self.__id = carId
        self.__best = {}
        self.__last = {}
        self.__numInterm = 0
        handlers = {('init', 'best_lap'): self._newBestLap,
                    ('init', 'best_time'): self._newBestTime,
                    ('init', 'chrono'): self._newChrono}
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

    def _newBestLap(self, car, _lap, time):
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
