from PyQt4.Qt import Qt
from PyQt4.QtCore import QAbstractTableModel, QVariant
from OTPApplication import OTPApplication


class WeatherModel(QAbstractTableModel):

    def __init__(self):
        QAbstractTableModel.__init__(self)
        self.__changes = {}
        self.__weather = {}
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
        if index.column() == 2 and role == Qt.EditRole:
            try:
                return QVariant(self.__changes[index.row()])
            except KeyError:
                return QVariant(self.__weather[index.row()])
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
