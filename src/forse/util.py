import hashlib, os, random, socket, sys
import qt4reactor, twotp
from twotp.term import Atom
from PyQt4.QtCore import QProcess, QTimer, pyqtSignal
from PyQt4.QtGui import QApplication


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


def buildNodeName(name, randomize=False):
    """
    Returns a string that can be used as the name of an Erlang or Python node.
    If C{randomize} is true, a random hexadecimal hash is appended to C{name}.
    """
    if not name.startswith("forse_"):
        name = "forse_" + name
    if randomize:
        name += '_' + randomHash()
    if '@' not in name:
        name += '@' + socket.gethostname()
    return name


def randomHash(length=8):
    """
    Generates a random hexadecimal string of the specified length.
    """
    return hashlib.sha1(str(random.random())).hexdigest()[:length]


class _ProxyHandler(object):

    def __init__(self):
        object.__init__(self)
        self.__handlers = {}

    def createHandler(self, name, method):
        setattr(self, "remote_" + name, method)

    def installHandler(self, tag, method):
        if tag not in self.__handlers:
            self.__handlers[tag] = []
        self.__handlers[tag].append(method)

    def remote_handleMessage(self, type, msg):
        try:
            for h in self.__handlers[type.text, msg[0].text]:
                h(*msg[1:])
        except KeyError:
            print "No handlers registered for", type.text, "message:", msg


class NodeApplication(QApplication):
    """
    Provides integration between a QApplication instance and a Python
    node, making easier to perform remote operations on Erlang nodes.
    """

    def __init__(self, appName):
        QApplication.__init__(self, sys.argv)
        qt4reactor.install()
        self._appName = appName
        self.__cookie = os.getenv('FORSE_COOKIE')
        if not self.__cookie:
            self.__cookie = twotp.readCookie()
        self.__nameServer = os.getenv('FORSE_NS')
        self.__nodeName = buildNodeName(appName, randomize=True)
        self.__process = twotp.Process(self.__nodeName, self.__cookie)
        self.__proxy = _ProxyHandler()
        QTimer.singleShot(0, self.__startup)

    def createHandler(self, name, method):
        self.__proxy.createHandler(name, method)

    def nodeName(self):
        return Atom(self.__nodeName)

    def registerMsgHandlers(self, handlers):
        for tag, method in handlers.iteritems():
            self.__proxy.installHandler(tag, method)

    def rpc(self, mod, fun, *args):
        return self.__process.callRemote(self.__nameServer, mod, fun, *args)

    def spawnErlangNode(self, runApp, nodeName=None, randomize=False):
        nodeName = buildNodeName(nodeName if nodeName else runApp, randomize)
        self.__nameServer = nodeName
        args = ["-config", "forse",
                "-detached",
                "-pa", "ebin",
                "-setcookie", self.__cookie,
                "-sname", nodeName,
                "-run", runApp]
        return QProcess.startDetached("erl", args)

    def __startup(self):
        from twisted.internet import reactor
        reactor.runReturn()
        self.__process.register(self._appName)
        self.__process.registerModule(self._appName, self.__proxy)
        self.__process.listen()


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
        self.__retryDelay = 1
        self.__retrySubscription()

    def subscribe(self):
        """
        Sends a subscription request to the event_dispatcher.
        """
        cbargs = [self.nodeName(), Atom(self._appName), Atom("handleMessage")]
        callback = Atom("callback"), Atom("rpc"), Atom("call"), cbargs
        d = self.rpc("event_dispatcher", "subscribe", Atom(self._appName), callback)
        d.addCallback(self.__subscribeCB)
        d.addErrback(self.__subscribeEB)

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
