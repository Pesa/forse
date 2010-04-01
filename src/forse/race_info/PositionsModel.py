import OTPApplication
from PyQt4.Qt import Qt
from PyQt4.QtCore import QAbstractTableModel, QVariant
from PyQt4.QtGui import QIcon
from PilotInfo import PilotInfo


class PositionsModel(QAbstractTableModel):

    def __init__(self):
        QAbstractTableModel.__init__(self)
        self.__arrowDown = QIcon(":/icons/arrow-down.png")
        self.__arrowUp = QIcon(":/icons/arrow-up.png")
        self.__positions = {}
        handlers = {('init', 'standings'): self._standingsInit,
                    ('update', 'standings'): self._standingsUpdate}
        OTPApplication.registerMsgHandlers(handlers)

    def columnCount(self, _parent):
        return 2

    def rowCount(self, _parent):
        return len(self.__positions)

    def data(self, index, role):
        if role == Qt.DisplayRole:
            try:
                i = self.__positions[index.row() + 1]
                if index.column() == 0:
                    return QVariant(PilotInfo.get(i).name())
                elif index.column() == 1:
                    return QVariant(PilotInfo.get(i).state())
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
                    return QVariant(PilotInfo.get(self.__positions[section + 1]).icon())
                except KeyError:
                    pass
        elif role == Qt.TextAlignmentRole:
            if orientation == Qt.Vertical:
                return QVariant(Qt.AlignCenter)
        return QVariant()

    def _standingsInit(self, standings):
        self.__positions = dict([(pos, id) for id, pos in standings])
        self.reset()

    def _standingsUpdate(self, standings):
        for id, newpos in standings:
            for oldpos, pilot in self.__positions.iteritems():
                if pilot == id:
                    if newpos > oldpos:
                        PilotInfo.get(id).setIcon(self.__arrowDown)
                    elif newpos < oldpos:
                        PilotInfo.get(id).setIcon(self.__arrowUp)
                    break
        for id, newpos in standings:
            self.__positions[newpos] = id
        self.reset()
