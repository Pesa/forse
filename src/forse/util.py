import hashlib, os, qt4reactor, random, sys, twotp
from twisted.python.failure import Failure
from twotp.term import Atom
from PyQt4.QtCore import QObject, QTimer, pyqtSignal, pyqtSlot
from PyQt4.QtGui import QApplication


__all__ = ['atomToBool', 'RPC', 'NodeApplication', 'SubscriberApplication']


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


class RPCReply(object):

    def __init__(self, reply):
        object.__init__(self)
        if isinstance(reply, Atom):
            self.__reply = reply.text
        elif isinstance(reply, Failure):
            self.__reply = "<%s.%s> : %s" % (reply.type.__module__,
                                             reply.type.__name__,
                                             reply.value)
        else:
            self.__reply = reply

    def __str__(self):
        return str(self.__reply)


class RPC(QObject):
    """
    Encapsulates the execution of C{mod:fun(args)} as a remote procedure call.
    """

    done = pyqtSignal(RPCReply)

    def __init__(self, mod, fun, *args):
        QObject.__init__(self)
        self.__mod = mod
        self.__fun = fun
        self.__args = args

    @pyqtSlot()
    def call(self, *args):
        """
        Performs the actual remote call. The signal C{done} is emitted
        when a reply has been received from the remote side.
        """
        if not args:
            args = self.__args
        d = NodeApplication.instance()._rpc(self.__mod, self.__fun, *args)
        d.addBoth(lambda x: self.done.emit(RPCReply(x)))


class NodeApplication(QApplication):
    """
    Provides integration between a QApplication instance and a Python
    node, making easier to perform remote operations on Erlang nodes.
    """

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

    def _rpc(self, mod, fun, *args):
        return self.__process.callRemote(self.__nameServer, mod, fun, *args)

    def _registerModule(self, name, module):
        self.__process.registerModule(name, module)

    def _startupHook(self):
        pass

    def __generateRandomHash(self, length=8):
        return hashlib.sha1(str(random.random())).hexdigest()[:length]

    def __startup(self):
        from twisted.internet import reactor
        reactor.runReturn()
        self.__process.register(self._appName)
        self.__process.listen()
        self._startupHook()


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
