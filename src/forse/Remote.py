import OTPApplication
from twisted.python.failure import Failure
from twotp import Atom


__all__ = ['BootstrapServer', 'EventDispatcher',
           'NodeManager', 'Team', 'Scheduler']


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


class _RPCTo(object):
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
        d = OTPApplication.rpc(self.__mod, self.__fun, *args)
        if callback is not None:
            d.addBoth(lambda x: callback(_RPCReply(x)))


class _RPCFrom(object):
    """
    Provides a thin abstraction layer (and a more pythonic interface)
    for a remote procedure call received by a Python node.
    """

    def __init__(self, notif):
        object.__init__(self)
        self.__notif = notif

    def connect(self, handler):
        def wrapper(*args):
            handler(*[ _RPCReply(x) for x in args ])
        OTPApplication.createHandler(self.__notif, wrapper)


class BootstrapServer(object):

    bootstrap = _RPCTo("bootstrap_server", "bootstrap")
    readConfigFiles = _RPCTo("bootstrap_server", "read_config_files")
    setGuiNode = _RPCTo("bootstrap_server", "set_gui_node")
    start = staticmethod(lambda: OTPApplication.spawnErlangNode("bootstrap_server"))
    stop = _RPCTo("init", "stop")

    nodeDown = _RPCFrom("node_down")
    nodeUp = _RPCFrom("node_up")
    notReady = _RPCFrom("not_ready")
    ready = _RPCFrom("ready")


class EventDispatcher(object):

    subscribe = _RPCTo("event_dispatcher", "subscribe")


class NodeManager(object):

    configure = _RPCTo("node_manager", "configure")
    start = staticmethod(lambda: OTPApplication.spawnErlangNode("node_manager", "node", randomize=True))


class Team(object):
    pass


class Scheduler(object):

    setSpeedup = _RPCTo("scheduler", "set_speedup")
    startSimulation = _RPCTo("scheduler", "start_simulation")
    pauseSimulation = _RPCTo("scheduler", "pause_simulation")