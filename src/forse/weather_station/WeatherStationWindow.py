from PyQt4.Qt import Qt
from PyQt4.QtGui import QMainWindow
from Subscriber import SubscriberApplication
from WeatherModel import WeatherModel
from Ui_WeatherStationWindow import Ui_WeatherStationWindow


class WeatherStationWindow(QMainWindow, Ui_WeatherStationWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self.__model = WeatherModel()
        self.__model.modelReset.connect(self.weatherTable.resizeColumnsToContents, Qt.QueuedConnection)
        self.weatherTable.setModel(self.__model)
        self.weatherView.sectorClicked.connect(self.weatherTable.selectRow)
        handlers = {('update', 'weather'): self._weatherChanged}
        SubscriberApplication.registerMsgHandlers(handlers)
        SubscriberApplication.instance().subscribed.connect(self.statusBar().clearMessage)
        SubscriberApplication.instance().subscriptionError.connect(self._subscriptionError)
        SubscriberApplication.instance().subscribe()

    def _subscriptionError(self):
        self.statusBar().showMessage("Subscription failed, retrying ...", 1500)

    def _weatherChanged(self, weather):
        msg = "sector" + ("s " if len(weather) > 1 else " ")
        msg += ', '.join([str(s) for s, _ in weather])
        self.statusBar().showMessage("Weather changed in " + msg, 10000)
