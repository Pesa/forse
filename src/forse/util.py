import os, sys, twotp
from hashlib import sha256
from random import random
from twotp.term import Atom
from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QApplication


__all__ = ['atomToBool', 'NodeApplication']


def atomToBool(atom):
    if not isinstance(atom, Atom):
        raise TypeError(str(atom) + " is not an atom.")
    if atom.text == "true":
        return True
    elif atom.text == "false":
        return False
    else:
        raise ValueError(str(atom) + " is not a valid boolean value.")


class ProxyHandler(object):

    def __init__(self):
        object.__init__(self)
        self._handlers = {}

    def remote_handle_msg(self, type, msg):
        try:
            key, value = msg
            for h in self._handlers[type.text, key.text]:
                h(value)
        except KeyError:
            print "No handlers registered for", type.text, "message:", msg


class NodeApplication(QApplication):

    def __init__(self, appName):
        QApplication.__init__(self, sys.argv)
        self.__nameServer = os.getenv("FORSE_NS")
        if not self.__nameServer:
            raise ValueError("environment variable FORSE_NS is not defined.")
        self.__appName = appName
        self.__cookie = twotp.readCookie()
        self.__nodeName = twotp.buildNodeName(appName + "_" + self.__generateRandomHash())
        self.__process = twotp.Process(self.__nodeName, self.__cookie)
        self.__proxy = ProxyHandler()
        self.__retryDelay = 1
        QTimer.singleShot(0, self.__startup)

    def __connect(self):
        d = self.__process.callRemote(self.__nameServer, "global", "whereis_name",
                                      Atom("event_dispatcher"))
        d.addCallback(self.__resolveCB)
        d.addErrback(self.__resolveEB)

    def __generateRandomHash(self, length=8):
        return sha256(str(random())).hexdigest()[:length]

    def __nodeCB(self, result):
        args = [Atom(self.__nodeName), Atom(self.__appName), Atom("handle_msg")]
        callback = Atom("callback"), Atom("rpc"), Atom("call"), args
        d = self.__process.callRemote(result.text, "event_dispatcher", "subscribe",
                                      Atom(self.__appName), callback)
        d.addCallback(self.__subscribeCB)
        d.addErrback(self.__subscribeEB)

    def __nodeEB(self, error):
        print error
        self.__retryConnect()

    def __resolveCB(self, result):
        d = self.__process.callRemote(self.__nameServer, "erlang", "node", result)
        d.addCallback(self.__nodeCB)
        d.addErrback(self.__nodeEB)

    def __resolveEB(self, error):
        print error
        self.__retryConnect()

    def __retryConnect(self):
        QTimer.singleShot(self.__retryDelay * 1000, self.__connect)
        if self.__retryDelay < 10:
            self.__retryDelay += 1

    def __startup(self):
        import qt4reactor
        qt4reactor.install()
        from twisted.internet import reactor
        reactor.runReturn()
        self.__process.register(self.__appName)
        self.__process.registerModule(self.__appName, self.__proxy)
        self.__process.listen()
        self.__connect()

    def __subscribeCB(self, result):
        if isinstance(result, Atom) and result.text == "ok":
            self.__retryDelay = 1
        else:
            print "Subscription failed:", result

    def __subscribeEB(self, error):
        print error

    def registerMsgHandlers(self, handlers):
        for tag, method in handlers.iteritems():
            if tag not in self.__proxy._handlers:
                self.__proxy._handlers[tag] = []
            self.__proxy._handlers[tag].append(method)
