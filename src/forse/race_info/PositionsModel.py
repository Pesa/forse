from PyQt4.Qt import Qt
from PyQt4.QtCore import QAbstractTableModel, QVariant
from util import NodeApplication


class PositionsModel(QAbstractTableModel):

    def __init__(self):
        QAbstractTableModel.__init__(self)
        handlers = {}
        NodeApplication.instance().registerMsgHandlers(handlers)

    def columnCount(self, _parent):
        return 2

    def rowCount(self, _parent):
        return 10

    def data(self, index, role):
        if index.isValid() and role == Qt.DisplayRole:
            return QVariant("foo")
        return QVariant()
