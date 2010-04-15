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
