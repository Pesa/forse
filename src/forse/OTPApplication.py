import os, qt4reactor, sys, twotp
from twotp.term import Atom
from PyQt4.QtCore import QProcess, QTimer
from PyQt4.QtGui import QApplication
from Util import buildNodeName


__all__ = ['OTPApplication']


class _ProxyHandler(object):

    def __init__(self):
        object.__init__(self)
        self.__handlers = {}

    def clearHandlers(self):
        self.__handlers = {}

    def createHandler(self, name, method):
        name = "remote_" + name
        if not hasattr(self, name):
            setattr(self, name, method)

    def installHandler(self, tag, method):
        if tag not in self.__handlers:
            self.__handlers[tag] = []
        self.__handlers[tag].append(method)

    def remote_handleMessage(self, kind, msg):
        if not isinstance(kind, Atom):
            return
        # specific message handlers
        try:
            handlers = self.__handlers[kind.text, msg[0].text]
        except (AttributeError, LookupError):
            pass
        else:
            for h in handlers:
                h(*msg[1:])
        # generic message handlers
        try:
            handlers = self.__handlers[kind.text]
        except KeyError:
            pass
        else:
            for h in handlers:
                h(msg)


class OTPApplication(QApplication):
    """
    Provides integration between a QApplication instance and a Python
    node, making easier to perform remote operations on Erlang nodes.
    """

    _appName = None
    _cookie = os.getenv('FORSE_COOKIE')
    if not _cookie:
        _cookie = twotp.readCookie()
    _erlBinary = os.getenv('ERL')
    _erlProcess = None
    _initialized = False
    _nameServer = os.getenv('FORSE_NS')
    _nodeName = None
    _process = None
    proxy = _ProxyHandler()

    def __init__(self, appName):
        QApplication.__init__(self, sys.argv)
        if self._initialized:
            return
        qt4reactor.install()
        OTPApplication._appName = appName
        OTPApplication._nodeName = buildNodeName(self._appName, randomize=True)
        OTPApplication._process = twotp.Process(self._nodeName, self._cookie)
        QTimer.singleShot(0, self._startup)
        OTPApplication._initialized = True

    def _startup(self):
        from twisted.internet import reactor
        reactor.runReturn()
        self._process.register(self._appName)
        self._process.registerModule(self._appName, self.proxy)
        self._process.listen()

    @classmethod
    def appName(cls):
        """
        @return: the application name
        """
        return Atom(cls._appName)

    @classmethod
    def nodeName(cls):
        """
        @return: the name of the Python node
        """
        return Atom(cls._nodeName)

    @classmethod
    def registerMsgHandlers(cls, handlers):
        for tag, method in handlers.iteritems():
            cls.proxy.installHandler(tag, method)

    @classmethod
    def removeAllHandlers(cls):
        cls.proxy.clearHandlers()

    @classmethod
    def rpc(cls, mod, fun, *args):
        return cls._process.callRemote(cls._nameServer, mod, fun, *args)

    @classmethod
    def spawnErlangNode(cls, runApp, nodeName=None, randomize=False):
        if cls._erlProcess is not None:
            raise RuntimeError("spawning more than one Erlang node is not supported.")
        sname = buildNodeName(nodeName if nodeName else runApp, randomize)
        cls._nameServer = sname
        args = ["-config", "forse",
                "-noinput",
                "-pa", "ebin",
                "-setcookie", cls._cookie,
                "-sname", sname,
                "-run", runApp]
        cls._erlProcess = QProcess()
        cls._erlProcess.setProcessChannelMode(QProcess.ForwardedChannels)
        cls._erlProcess.start(cls._erlBinary, args, QProcess.ReadOnly)
        return cls._erlProcess.waitForStarted(10000)
