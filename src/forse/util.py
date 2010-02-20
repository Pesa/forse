import hashlib, os, qt4reactor, random, sys, twotp
from twotp.term import Atom
from PyQt4.QtCore import QTimer, pyqtSignal
from PyQt4.QtGui import QApplication


__all__ = ['atomToBool', 'NodeApplication', 'SubscriberApplication']


def atomToBool(atom):
    """
    Translates an Erlang atom to a Python boolean value.
    Raises an exception if the conversion fails.
    """
    if not isinstance(atom, Atom):
        raise TypeError(str(atom) + " is not an atom.")
    if atom.text == "true":
        return True
    elif atom.text == "false":
        return False
    else:
        raise ValueError(str(atom) + " is not a valid boolean value.")


class _ProxyHandler(object):

    def __init__(self):
        object.__init__(self)
        self.__handlers = {}

    def addHandler(self, tag, method):
        if tag not in self.__handlers:
            self.__handlers[tag] = []
        self.__handlers[tag].append(method)

    def remote_handleMessage(self, type, msg):
        try:
            key, value = msg
            for h in self.__handlers[type.text, key.text]:
                h(value)
        except KeyError:
            print "No handlers registered for", type.text, "message:", msg


class NodeApplication(QApplication):
    """
    Provides integration between a QApplication instance and a Python
    node, making easier to perform remote operations on Erlang nodes.
    """

    rpcError = pyqtSignal()
    rpcResult = pyqtSignal(str)

    def __init__(self, appName):
        QApplication.__init__(self, sys.argv)
        qt4reactor.install()
        self._appName = appName
        self.__cookie = os.getenv("FORSE_COOKIE")
        if not self.__cookie:
            self.__cookie = twotp.readCookie()
        self.__nameServer = os.getenv("FORSE_NS")
        if not self.__nameServer:
            raise ValueError("environment variable FORSE_NS is not defined.")
        self._nodeName = twotp.buildNodeName(appName + "_" + self.__generateRandomHash())
        self.__process = twotp.Process(self._nodeName, self.__cookie)
        QTimer.singleShot(0, self.__startup)

    def rpc(self, mod, fun, *args):
        """
        Executes C{mod:fun(args)} as a remote procedure call.
        """
        d = self._rpc(mod, fun, *args)
        d.addCallback(self.__rpcCB)
        d.addErrback(self.__rpcEB)

    def _rpc(self, mod, fun, *args):
        return self.__process.callRemote(self.__nameServer, mod, fun, *args)

    def _registerModule(self, name, module):
        self.__process.registerModule(name, module)

    def __generateRandomHash(self, length=8):
        return hashlib.sha1(str(random.random())).hexdigest()[:length]

    def __rpcCB(self, result):
        if isinstance(result, Atom):
            self.rpcResult.emit(result.text)
        else:
            raise TypeError(str(result) + " is not supported.")

    def __rpcEB(self, _error):
        self.rpcError.emit()

    def __startup(self):
        from twisted.internet import reactor
        reactor.runReturn()
        self.__process.register(self._appName)
        self.__process.listen()
        self._startupHook()

    def _startupHook(self):
        pass


class SubscriberApplication(NodeApplication):
    """
    NodeApplication subclass which can subscribe to an event_dispatcher
    service. After subscribing, events are automatically forwarded to
    a set of application-defined handlers.
    """

    subscribed = pyqtSignal()
    subscriptionError = pyqtSignal()

    def __init__(self, appName):
        NodeApplication.__init__(self, appName)
        self.__proxy = _ProxyHandler()
        self.__retryDelay = 1

    def registerMsgHandlers(self, handlers):
        for tag, method in handlers.iteritems():
            self.__proxy.addHandler(tag, method)

    def subscribe(self):
        """
        Sends a subscription request to the event_dispatcher.
        """
        cbargs = [Atom(self._nodeName), Atom(self._appName), Atom("handleMessage")]
        callback = Atom("callback"), Atom("rpc"), Atom("call"), cbargs
        d = self._rpc("event_dispatcher", "subscribe", Atom(self._appName), callback)
        d.addCallback(self.__subscribeCB)
        d.addErrback(self.__subscribeEB)

    def _startupHook(self):
        self._registerModule(self._appName, self.__proxy)
        self.subscribe()

    def __retrySubscription(self):
        QTimer.singleShot(self.__retryDelay * 1000, self.subscribe)
        if self.__retryDelay < 10:
            self.__retryDelay += 1

    def __subscribeCB(self, result):
        if isinstance(result, Atom) and result.text == "ok":
            self.__retryDelay = 1
            self.subscribed.emit()
        else:
            self.subscriptionError.emit()

    def __subscribeEB(self, _error):
        self.subscriptionError.emit()
        self.__retrySubscription()
