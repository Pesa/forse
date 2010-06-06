###########################################################################
#
# Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
#
# This file is part of FORSE.
#
# FORSE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# FORSE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
#
###########################################################################

from twisted.python.failure import Failure
from twotp import Atom
from OTPApplication import OTPApplication


__all__ = ['BootstrapServer', 'Car', 'EventDispatcher',
           'NodeManager', 'Scheduler', 'Weather']


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
            s = ''.join([self.__toString(e, True) for e in x])
            if inErlangString:
                return s
            else:
                return '"%s"' % s
        elif isinstance(x, tuple):
            s = ', '.join([self.__toString(e) for e in x])
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
            handler(*[_RPCReply(x) for x in args])
        OTPApplication.proxy.createHandler(self.__notif, wrapper)


class BootstrapServer(object):

    start = staticmethod(lambda: OTPApplication.spawnErlangNode("bootstrap_server"))
    bootstrap = _RPCTo("bootstrap_server", "bootstrap")
    readConfigFiles = _RPCTo("bootstrap_server", "read_config_files")
    setGuiNode = _RPCTo("bootstrap_server", "set_gui_node")
    shutdown = _RPCTo("bootstrap_server", "shutdown")

    nodeDown = _RPCFrom("node_down")
    nodeUp = _RPCFrom("node_up")
    notReady = _RPCFrom("not_ready")
    ready = _RPCFrom("ready")


class Car(object):

    forcePitstop = _RPCTo("car", "force_pitstop")
    retire = _RPCTo("car", "retire")


class EventDispatcher(object):

    subscribe = _RPCTo("event_dispatcher", "subscribe")


class NodeManager(object):

    start = staticmethod(lambda: OTPApplication.spawnErlangNode("node_manager", "node", randomize=True))
    configure = _RPCTo("node_manager", "configure")


class Scheduler(object):

    setSpeedup = _RPCTo("scheduler", "set_speedup")
    startSimulation = _RPCTo("scheduler", "start_simulation")
    pauseSimulation = _RPCTo("scheduler", "pause_simulation")


class Weather(object):

    scheduleChange = _RPCTo("weather", "schedule_change")
