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

    def __init__(self, appName, autoSubscribe=True):
        OTPApplication.__init__(self, appName)
        self.__opts = None
        self.__retryDelay = 1
        if autoSubscribe:
            self.__retrySubscription()

    def setSubscriptionOptions(self, opts):
        self.__opts = opts

    def subscribe(self):
        """
        Sends a subscription request to the event_dispatcher.
        """
        cbargs = [nodeName(), appName(), Atom("handleMessage")]
        callback = Atom("callback"), Atom("rpc"), Atom("call"), cbargs
        if isinstance(self.__opts, list):
            EventDispatcher.subscribe(self.__subscribeDone, appName(), callback, self.__opts)
        else:
            EventDispatcher.subscribe(self.__subscribeDone, appName(), callback)

    def __retrySubscription(self):
        QTimer.singleShot(self.__retryDelay * 1000, self.subscribe)
        if self.__retryDelay < 10:
            self.__retryDelay += 1

    def __subscribeDone(self, reply):
        if reply == "ok":
            self.__retryDelay = 1
            self.subscribed.emit()
        else:
            self.subscriptionError.emit()
            self.__retrySubscription()
