import os, qt4reactor, sys, twotp
from twotp.term import Atom
from PyQt4.QtCore import QProcess, QTimer
from PyQt4.QtGui import QApplication
import Util


__all__ = ['OTPApplication', 'appName', 'createHandler', 'nodeName',
           'registerMsgHandlers', 'rpc', 'spawnErlangNode']


def appName():
    """
    @return: the application name
    """
    return Atom(_Global.appName)


def createHandler(name, method):
    _Global.proxy.createHandler(name, method)


def nodeName():
    """
    @return: the name of the Python node
    """
    return Atom(_Global.nodeName)


def registerMsgHandlers(handlers):
    for tag, method in handlers.iteritems():
        _Global.proxy.installHandler(tag, method)


def rpc(mod, fun, *args):
    return _Global.process.callRemote(_Global.nameServer, mod, fun, *args)


def spawnErlangNode(runApp, nodeName=None, randomize=False):
    if _Global.erlProcess is not None:
        raise RuntimeError("spawning more than one Erlang node is not supported.")
    sname = Util.buildNodeName(nodeName if nodeName else runApp, randomize)
    _Global.nameServer = sname
    args = ["-config", "forse",
            "-noinput",
            "-pa", "ebin",
            "-setcookie", _Global.cookie,
            "-sname", sname,
            "-run", runApp]
    _Global.erlProcess = QProcess()
    _Global.erlProcess.setProcessChannelMode(QProcess.ForwardedChannels)
    _Global.erlProcess.start("erl", args, QProcess.ReadOnly)
    return _Global.erlProcess.waitForStarted(10000)


class OTPApplication(QApplication):
    """
    Provides integration between a QApplication instance and a Python
    node, making easier to perform remote operations on Erlang nodes.
    """

    def __init__(self, appName):
        QApplication.__init__(self, sys.argv)
        qt4reactor.install()
        _Global.init(appName)


class _ProxyHandler(object):

    def __init__(self):
        object.__init__(self)
        self.__handlers = {}

    def createHandler(self, name, method):
        # FIXME: previous handlers are overwritten!
        setattr(self, "remote_" + name, method)

    def installHandler(self, tag, method):
        if tag not in self.__handlers:
            self.__handlers[tag] = []
        self.__handlers[tag].append(method)

    def remote_handleMessage(self, type, msg):
        if not isinstance(type, Atom):
            return
        handled = False
        # specific message handlers
        try:
            handlers = self.__handlers[type.text, msg[0].text]
        except (AttributeError, LookupError):
            pass
        else:
            for h in handlers:
                h(*msg[1:])
            handled = True
        # generic message handlers
        try:
            handlers = self.__handlers[type.text]
        except KeyError:
            pass
        else:
            for h in handlers:
                h(msg)
            handled = True
        if not handled:
            print "No handlers registered for", type.text, "message:", msg


class _Global(object):

    appName = None
    cookie = os.getenv('FORSE_COOKIE')
    if not cookie:
        cookie = twotp.readCookie()
    erlProcess = None
    nameServer = os.getenv('FORSE_NS')
    nodeName = None
    process = None
    proxy = _ProxyHandler()
    _initialized = False

    @classmethod
    def init(cls, appName):
        if cls._initialized:
            return
        cls.appName = appName
        cls.nodeName = Util.buildNodeName(cls.appName, randomize=True)
        cls.process = twotp.Process(cls.nodeName, cls.cookie)
        QTimer.singleShot(0, cls._startup)
        cls._initialized = True

    @classmethod
    def _startup(cls):
        from twisted.internet import reactor
        reactor.runReturn()
        cls.process.register(cls.appName)
        cls.process.registerModule(cls.appName, cls.proxy)
        cls.process.listen()
