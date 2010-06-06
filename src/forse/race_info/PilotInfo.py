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

from PyQt4.QtCore import QTimer
from OTPApplication import OTPApplication
from Util import listToString


class PilotInfo(object):

    __info = {}

    def __init__(self):
        object.__init__(self)
        self._name = None
        self._teamName = None
        self._state = None
        self._icon = None
        self._timer = QTimer()
        self._timer.setInterval(5000)
        self._timer.setSingleShot(True)
        self._timer.timeout.connect(self._timerExpired)

    @classmethod
    def init(cls, refreshFunc):
        cls._refresh = refreshFunc
        handlers = {('init', 'cars_state'): cls._handleCarsState,
                    ('init', 'names'): cls._handleNames}
        OTPApplication.registerMsgHandlers(handlers)

    @classmethod
    def get(cls, key):
        try:
            return cls.__info[key]
        except KeyError:
            return PilotInfo()

    def name(self):
        return "N/A" if self._name is None else self._name

    def teamName(self):
        return "N/A" if self._teamName is None else self._teamName

    def state(self):
        return "unknown" if self._state is None else self._state

    def icon(self):
        return self._icon

    def setIcon(self, icon):
        self._icon = icon
        self._timer.start()

    @classmethod
    def _handleCarsState(cls, states):
        for pilot, state in states:
            if pilot not in cls.__info:
                cls.__info[pilot] = PilotInfo()
            cls.__info[pilot]._state = state.text
        cls._refresh()

    @classmethod
    def _handleNames(cls, names):
        for pilot, name, teamName in names:
            if pilot not in cls.__info:
                cls.__info[pilot] = PilotInfo()
            cls.__info[pilot]._name = listToString(name)
            cls.__info[pilot]._teamName = listToString(teamName)
        cls._refresh()

    def _timerExpired(self):
        self._icon = None
        self._refresh()
