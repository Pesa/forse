from PyQt4.QtGui import QMainWindow
from PositionsModel import PositionsModel
from Remote import Scheduler
from Ui_RaceInfoWindow import Ui_RaceInfoWindow


class RaceInfoWindow(QMainWindow, Ui_RaceInfoWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self._switchButtonToStart()
        self._posModel = PositionsModel()
        self.positionsView.setModel(self._posModel)

    def pauseSimulation(self):
        self.startpauseButton.setEnabled(False)
        self.startpauseButton.clicked.disconnect()
        Scheduler.pauseSimulation(self._switchButtonToStart)

    def startSimulation(self):
        self.startpauseButton.setEnabled(False)
        self.startpauseButton.clicked.disconnect()
        Scheduler.startSimulation(self._switchButtonToPause)

    def _switchButtonToPause(self, reply="ok"):
        if reply == "ok":
            self.startpauseButton.clicked.connect(self.pauseSimulation)
            self.startpauseButton.setText("Pause simulation")
            self.startpauseButton.setEnabled(True)
        else:
            self.statusBar.showMessage(str(reply), 10000)

    def _switchButtonToStart(self, reply="ok"):
        if reply == "ok":
            self.startpauseButton.clicked.connect(self.startSimulation)
            self.startpauseButton.setText("Start simulation")
            self.startpauseButton.setEnabled(True)
        else:
            self.statusBar.showMessage(str(reply), 10000)
