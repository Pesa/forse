import OTPApplication, Util
from PyQt4.Qt import Qt
from PyQt4.QtCore import QAbstractTableModel, QVariant
from PilotInfo import PilotInfo


class TelemetryModel(QAbstractTableModel):

    def __init__(self):
        QAbstractTableModel.__init__(self)
        self.__data = {}
        self.__intermediate = None
        self.__lap = None
        handlers = {('init', 'chrono'): self._initChrono,
                    ('update', 'chrono'): self._updateChrono}
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
                    prefix = "" if index.row() == 0 else "+ "
                    return QVariant(prefix + Util.secondsToString(t[1]))
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
        return QVariant()

    def setIntermediate(self, intermediate):
        self.__intermediate = intermediate
        self.reset()

    def setLap(self, lap):
        self.__lap = lap
        self.reset()

    def _initChrono(self, car, intermediate, lap, reltime, time):
        if lap not in self.__data:
            self.__data[lap] = {}
        self.__data[lap][intermediate] = [(car, reltime, time)]
        self.__intermediate = intermediate
        self.__lap = lap
        self.reset()

    def _updateChrono(self, car, intermediate, lap, time):
        try:
            delta = time - self.__data[lap][intermediate][0][2]
            self.__data[lap][intermediate].append((car, delta))
            if lap == self.__lap and intermediate == self.__intermediate:
                self.reset()
        except KeyError:
            pass
