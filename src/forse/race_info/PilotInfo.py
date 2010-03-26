import OTPApplication
from PyQt4.QtCore import QString, QTimer
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
    def init(cls, refreshFunc=None):
        if refreshFunc is not None:
            global _refresh
            _refresh = refreshFunc
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
        for id, state in states:
            if id not in cls.__info:
                cls.__info[id] = PilotInfo()
            cls.__info[id]._state = state.text
        _refresh()

    @classmethod
    def _handleNames(cls, names):
        for id, name, teamName in names:
            if id not in cls.__info:
                cls.__info[id] = PilotInfo()
            cls.__info[id]._name = QString.fromUtf8(listToString(name))
            cls.__info[id]._teamName = QString.fromUtf8(listToString(teamName))
        _refresh()

    @staticmethod
    def _refresh():
        pass

    def _timerExpired(self):
        self._icon = None
        _refresh()
