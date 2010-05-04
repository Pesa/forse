from PyQt4.QtGui import QMainWindow
from Subscriber import SubscriberApplication
from Ui_WeatherStationWindow import Ui_WeatherStationWindow


class WeatherStationWindow(QMainWindow, Ui_WeatherStationWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        SubscriberApplication.instance().subscribed.connect(self.statusBar().clearMessage)
        SubscriberApplication.instance().subscriptionError.connect(self._subscriptionError)
        SubscriberApplication.instance().subscribe()

    def _subscriptionError(self):
        self.statusBar().showMessage("Subscription failed, retrying ...", 1500)
