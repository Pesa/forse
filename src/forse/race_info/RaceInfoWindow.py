from PyQt4.QtGui import QMainWindow
from PositionsModel import PositionsModel
from Ui_RaceInfoWindow import Ui_RaceInfoWindow


class RaceInfoWindow(QMainWindow, Ui_RaceInfoWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self._posModel = PositionsModel()
        self.positionsView.setModel(self._posModel)