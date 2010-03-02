from twisted.python.failure import Failure
from twotp import Atom
from util import NodeApplication


class _RPCReply(object):
    """
    Contains the reply returned by the remote node during a RPC.
    """

    def __init__(self, reply):
        object.__init__(self)
        self.__reply = self.__toString(reply)

    def __eq__(self, other):
        return self.__reply == other

    def __ne__(self, other):
        return self.__reply != other

    def __str__(self):
        return self.__reply

    def __toString(self, x, inErlangString=False):
        if isinstance(x, Atom):
            return x.text
        elif isinstance(x, Failure):
            return '<%s.%s> : %s' % (x.type.__module__, x.type.__name__, x.value)
        elif isinstance(x, int) and inErlangString:
            try:
                return chr(x)
            except ValueError:
                return str(x)
        elif isinstance(x, list):
            s = ''.join([ self.__toString(e, True) for e in x ])
            if inErlangString:
                return s
            else:
                return '"%s"' % s
        elif isinstance(x, tuple):
            s = ', '.join([ self.__toString(e) for e in x ])
            return '{%s}' % s
        else:
            return str(x)


class _RPC(object):
    """
    Encapsulates the execution of C{mod:fun(args)} as a remote procedure call.
    """

    def __init__(self, mod, fun):
        object.__init__(self)
        self.__mod = mod
        self.__fun = fun

    def __call__(self, callback, *args):
        """
        Performs the actual remote call. The callable object C{callback} is
        invoked as soon as a reply has been received from the remote side.
        """
        d = NodeApplication.instance().rpc(self.__mod, self.__fun, *args)
        d.addBoth(lambda x: callback(_RPCReply(x)))


class BootstrapServer(object):

    bootstrap = _RPC("bootstrap_server", "bootstrap")
    readConfigFiles = _RPC("bootstrap_server", "read_config_files")
    stop = _RPC("init", "stop")

    @staticmethod
    def start():
        return NodeApplication.instance().spawnErlangNode("bootstrap_server")


class NodeManager(object):

    configure = _RPC("node_manager", "configure")

    @staticmethod
    def start():
        return NodeApplication.instance().spawnErlangNode("node_manager", "node", randomize=True)


class Team(object):
    pass
