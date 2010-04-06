from PyQt4.QtGui import QMainWindow
from Subscriber import SubscriberApplication
from Util import listToString
from Ui_LoggerWindow import Ui_LoggerWindow


class LoggerWindow(QMainWindow, Ui_LoggerWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        handlers = {'init': self._setLogMsg,
                    'update': self._appendLogMsg}
        SubscriberApplication.registerMsgHandlers(handlers)
        SubscriberApplication.instance().subscribed.connect(self.statusBar().clearMessage)
        SubscriberApplication.instance().subscriptionError.connect(self._subscriptionError)
        SubscriberApplication.instance().subscribe()

    def _appendLogMsg(self, msg):
        self.viewer.appendPlainText(listToString(msg))

    def _setLogMsg(self, msg):
        self.viewer.setPlainText(listToString(msg))

    def _subscriptionError(self):
        self.statusBar().showMessage("Subscription failed", 5000)
