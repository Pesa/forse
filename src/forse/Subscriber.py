from twotp.term import Atom
from PyQt4.QtCore import QTimer, pyqtSignal
from OTPApplication import OTPApplication, appName, nodeName
from Remote import EventDispatcher


__all__ = ['SubscriberApplication']


class SubscriberApplication(OTPApplication):
    """
    OTPApplication subclass which can subscribe to an event_dispatcher
    service. After subscribing, events are automatically forwarded to
    a set of application-defined handlers.
    """

    subscribed = pyqtSignal()
    subscriptionError = pyqtSignal()

    def __init__(self, appName):
        OTPApplication.__init__(self, appName)
        self.__opts = None

    def setSubscriptionOptions(self, opts):
        self.__opts = opts

    def subscribe(self):
        QTimer.singleShot(0, self.__subscribe)

    def __subscribe(self):
        cbargs = [nodeName(), appName(), Atom("handleMessage")]
        callback = Atom("callback"), Atom("rpc"), Atom("call"), cbargs
        if isinstance(self.__opts, list):
            EventDispatcher.subscribe(self.__subscribeDone, appName(), callback, self.__opts)
        else:
            EventDispatcher.subscribe(self.__subscribeDone, appName(), callback)

    def __subscribeDone(self, reply):
        if reply == "ok":
            self.subscribed.emit()
        else:
            self.subscriptionError.emit()
            QTimer.singleShot(1000, self.__subscribe)
