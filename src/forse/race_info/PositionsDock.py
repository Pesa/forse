from PyQt4.QtGui import QDockWidget
from PositionsModel import PositionsModel
from Ui_PositionsDock import Ui_PositionsDock


class PositionsDock(QDockWidget, Ui_PositionsDock):

    def __init__(self, parent=None):
        QDockWidget.__init__(self, parent)
        self.setupUi(self)
        self.__model = PositionsModel()
        self.positionsView.setModel(self.__model)

    def reloadPilotInfo(self):
        self.positionsView.reset()