from PyQt4.Qt import Qt
from PyQt4.QtGui import QDialogButtonBox, QMainWindow
from Subscriber import SubscriberApplication
from Remote import Weather
from SpinBoxDelegate import SpinBoxDelegate
from WeatherModel import WeatherModel
from Ui_WeatherStationWindow import Ui_WeatherStationWindow


class WeatherStationWindow(QMainWindow, Ui_WeatherStationWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self.__model = WeatherModel()
        self.__model.dataChanged.connect(self._handleModelChanges)
        self.__model.modelReset.connect(self._handleModelChanges)
        self.__model.modelReset.connect(self.weatherTable.resizeColumnsToContents, Qt.QueuedConnection)
        self.weatherTable.setModel(self.__model)
        self.weatherTable.setItemDelegateForColumn(2, SpinBoxDelegate(self))
        self.buttonBox.clicked.connect(self._handleButtonsClick)
        self.weatherView.sectorClicked.connect(self.weatherTable.selectRow)
        handlers = {('update', 'weather'): self._weatherChanged}
        SubscriberApplication.registerMsgHandlers(handlers)
        SubscriberApplication.instance().subscribed.connect(self.statusBar().clearMessage)
        SubscriberApplication.instance().subscriptionError.connect(self._subscriptionError)
        SubscriberApplication.instance().subscribe()

    def _checkReply(self, reply):
        if reply != "ok":
            self.statusBar().showMessage(str(reply), 5000)

    def _handleButtonsClick(self, button):
        role = self.buttonBox.buttonRole(button)
        if role == QDialogButtonBox.ApplyRole:
            Weather.scheduleChange(self._checkReply, 0, self.__model.changes())
            self.__model.discardChanges()
        elif role == QDialogButtonBox.ResetRole:
            self.__model.discardChanges()

    def _handleModelChanges(self, _topLeft=None, _bottomRight=None):
        self.buttonBox.setEnabled(len(self.__model.changes()) > 0)

    def _subscriptionError(self):
        self.statusBar().showMessage("Subscription failed, retrying ...", 1500)

    def _weatherChanged(self, weather):
        msg = "sector" + ("s " if len(weather) > 1 else " ")
        msg += ', '.join([str(s) for s, _ in weather])
        self.statusBar().showMessage("Weather changed in " + msg, 10000)
