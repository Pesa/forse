from PyQt4.Qt import Qt
from PyQt4.QtCore import QAbstractTableModel, QVariant
from PyQt4.QtGui import QIcon
from OTPApplication import OTPApplication


class WeatherModel(QAbstractTableModel):

    def __init__(self):
        QAbstractTableModel.__init__(self)
        self.__changes = {}
        self.__weather = {}
        self.__sun = QIcon(":/icons/sun.png")
        self.__lightRain = QIcon(":/icons/light-rain.png")
        self.__rain = QIcon(":/icons/rain.png")
        self.__heavyRain = QIcon(":/icons/heavy-rain.png")
        self.__icons = [self.__sun]
        self.__icons[1:3] = [self.__lightRain] * 3
        self.__icons[4:7] = [self.__rain] * 4
        self.__icons[8:10] = [self.__heavyRain] * 3
        handlers = {('init', 'weather'): self._setWeather,
                    ('update', 'weather'): self._setWeather}
        OTPApplication.registerMsgHandlers(handlers)

    def changes(self):
        return self.__changes.items()

    def discardChanges(self):
        self.__changes = {}
        self.reset()

    def columnCount(self, _parent):
        return 3

    def rowCount(self, _parent):
        return len(self.__weather)

    def flags(self, index):
        flags = QAbstractTableModel.flags(self, index)
        if index.column() == 2:
            flags |= Qt.ItemIsEditable
        return flags

    def data(self, index, role):
        if role == Qt.DisplayRole:
            try:
                if index.column() == 0:
                    return QVariant(index.row())
                elif index.column() == 1:
                    return QVariant(self.__weather[index.row()])
                elif index.column() == 2:
                    return QVariant(self.__changes[index.row()])
            except KeyError:
                pass
        elif role == Qt.DecorationRole and index.column() == 1:
            return QVariant(self.__icons[self.__weather[index.row()]])
        elif role == Qt.EditRole and index.column() == 2:
            try:
                return QVariant(self.__changes[index.row()])
            except KeyError:
                return QVariant(self.__weather[index.row()])
        elif role == Qt.TextAlignmentRole:
            return QVariant(Qt.AlignCenter)
        return QVariant()

    def headerData(self, section, orientation, role):
        if orientation == Qt.Horizontal and role == Qt.DisplayRole:
            if section == 0:
                return QVariant("Sector")
            elif section == 1:
                return QVariant("Current")
            elif section == 2:
                return QVariant("Local changes")
        return QVariant()

    def setData(self, index, value, role):
        changed = False
        if index.column() == 2 and role == Qt.EditRole:
            r = index.row()
            if value != self.__weather[r]:
                self.__changes[r] = value
                changed = True
            elif r in self.__changes:
                del self.__changes[r]
                changed = True
        if changed:
            self.dataChanged.emit(index, index)
        return changed

    def _setWeather(self, weather):
        for sectId, rain in weather:
            self.__weather[sectId] = rain
            try:
                if self.__changes[sectId] == rain:
                    del self.__changes[sectId]
            except KeyError:
                pass
        self.reset()
