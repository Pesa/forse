from PyQt4.Qt import Qt
from PyQt4.QtCore import QAbstractTableModel, QString, QTimer, QVariant
from PyQt4.QtGui import QIcon
from util import listToString, NodeApplication


__all__ = ['PositionsModel']


class PilotInfo(object):

    def __init__(self, model):
        object.__init__(self)
        self.name = None
        self.state = None
        self._icon = None
        self._reset = model.reset
        self._timer = QTimer()
        self._timer.setInterval(5000)
        self._timer.timeout.connect(self._timerExpired)

    def icon(self):
        return self._icon

    def setIcon(self, icon):
        self._icon = icon
        self._timer.start()

    def _timerExpired(self):
        self._timer.stop()
        self._icon = None
        self._reset()


class PositionsModel(QAbstractTableModel):

    def __init__(self):
        QAbstractTableModel.__init__(self)
        self.__arrowDown = QIcon(":/icons/arrow-down.png")
        self.__arrowUp = QIcon(":/icons/arrow-up.png")
        self.__pilots = {}
        self.__positions = {}
        handlers = {('init', 'names'): self._setNames,
                    ('init', 'standings'): self._setStandings,
                    ('update', 'standings'): self._updateStandings}
        NodeApplication.instance().registerMsgHandlers(handlers)

    def columnCount(self, _parent):
        return 2

    def rowCount(self, _parent):
        return len(self.__positions)

    def data(self, index, role):
        if role == Qt.DisplayRole and index.isValid():
            try:
                if index.column() == 0:
                    return QVariant(self.__pilots[self.__positions[index.row() + 1]].name)
                elif index.column() == 1:
                    return QVariant(self.__pilots[self.__positions[index.row() + 1]].state)
            except KeyError:
                pass
        return QVariant()

    def headerData(self, section, orientation, role):
        if role == Qt.DisplayRole:
            if orientation == Qt.Horizontal:
                if section == 0:
                    return QVariant("Pilot")
                elif section == 1:
                    return QVariant("State")
            else:
                return QVariant(section + 1)
        elif role == Qt.DecorationRole:
            if orientation == Qt.Vertical:
                try:
                    return QVariant(self.__pilots[self.__positions[section + 1]].icon())
                except KeyError:
                    pass
        return QVariant()

    def _setNames(self, names):
        for id, name, _ in names:
            self.__pilots[id].name = QString.fromUtf8(listToString(name))
        self.reset()

    def _setStandings(self, standings):
        self.__positions.clear()
        for id, pos, state in standings:
            if id not in self.__pilots:
                self.__pilots[id] = PilotInfo(self)
            self.__pilots[id].state = state.text
            self.__positions[pos] = id
        self.reset()

    def _updateStandings(self, standings):
        for id, newpos, newstate in standings:
            for oldpos, pilot in self.__positions.iteritems():
                if pilot == id:
                    if newpos > oldpos:
                        self.__pilots[id].setIcon(self.__arrowDown)
                    elif newpos < oldpos:
                        self.__pilots[id].setIcon(self.__arrowUp)
                    break
        for id, newpos, newstate in standings:
            self.__pilots[id].state = newstate.text
            self.__positions[newpos] = id
        self.reset()
