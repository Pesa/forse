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

from twotp import Atom
from PyQt4.QtCore import QTimer, pyqtSignal
from OTPApplication import OTPApplication
from Remote import EventDispatcher


__all__ = ['SubscriberApplication']


class SubscriberApplication(OTPApplication):
    """
    OTPApplication subclass which can subscribe to an event_dispatcher
    service. After subscribing, events are automatically forwarded to
    a set of application-defined handlers.
    """

    subscribed = pyqtSignal()
    subscriptionError = pyqtSignal()

    def __init__(self, appName):
        OTPApplication.__init__(self, appName)
        self.__opts = None

    def setSubscriptionOptions(self, opts):
        self.__opts = opts

    def subscribe(self):
        """
        Sends a subscription request asynchronously.
        """
        QTimer.singleShot(0, self.__subscribe)

    def __subscribe(self):
        cbargs = [self.nodeName(), self.appName(), Atom("handleMessage")]
        callback = Atom("callback"), Atom("rpc"), Atom("call"), cbargs
        if isinstance(self.__opts, list):
            opts = [Atom(str(x)) for x in self.__opts]
            EventDispatcher.subscribe(self.__subscribeDone, self.appName(), callback, opts)
        else:
            EventDispatcher.subscribe(self.__subscribeDone, self.appName(), callback)

    def __subscribeDone(self, reply):
        if reply == "ok":
            self.subscribed.emit()
        else:
            self.subscriptionError.emit()
            QTimer.singleShot(2000, self.__subscribe)
