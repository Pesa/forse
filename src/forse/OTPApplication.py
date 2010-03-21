import os, qt4reactor, sys, time, twotp
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
    sname = Util.buildNodeName(nodeName if nodeName else runApp, randomize)
    _Global.nameServer = sname
    args = ["-config", "forse",
            "-detached",
            "-pa", "ebin",
            "-setcookie", _Global.cookie,
            "-sname", sname,
            "-run", runApp]
    result = QProcess.startDetached("erl", args)
    time.sleep(2) # FIXME
    return result


class OTPApplication(QApplication):
    """
    Provides integration between a QApplication instance and a Python
    node, making easier to perform remote operations on Erlang nodes.
    """

    def __init__(self, appName):
        QApplication.__init__(self, sys.argv)
        qt4reactor.install()
        _Global.init(appName)


class _Global(object):

    appName = None
    cookie = os.getenv('FORSE_COOKIE')
    if not cookie:
        cookie = twotp.readCookie()
    nameServer = os.getenv('FORSE_NS')
    nodeName = None
    process = None
    proxy = None
    _initialized = False

    @staticmethod
    def init(appName):
        if _Global._initialized:
            return
        _Global.appName = appName
        _Global.nodeName = Util.buildNodeName(_Global.appName, randomize=True)
        _Global.process = twotp.Process(_Global.nodeName, _Global.cookie)
        _Global.proxy = _ProxyHandler()
        QTimer.singleShot(0, _Global._startup)
        _Global._initialized = True

    @staticmethod
    def _startup():
        from twisted.internet import reactor
        reactor.runReturn()
        _Global.process.register(_Global.appName)
        _Global.process.registerModule(_Global.appName, _Global.proxy)
        _Global.process.listen()


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
        try:
            for h in self.__handlers[type.text, msg[0].text]:
                h(*msg[1:])
        except KeyError:
            print "No handlers registered for", type.text, "message:", msg
