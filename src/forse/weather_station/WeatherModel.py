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

    def columnCount(self, _parent):
        return 3

    def rowCount(self, _parent):
        return len(self.__weather)

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
        return QVariant()

    def headerData(self, section, orientation, role):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                if section == 0:
                    return QVariant("Sector")
                elif section == 1:
                    return QVariant("Current")
                elif section == 2:
                    return QVariant("Local changes")
        return QVariant()

    def _setWeather(self, weather):
        for sectId, rain in weather:
            self.__weather[sectId] = rain
        self.reset()
