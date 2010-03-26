import OTPApplication, Util
from PyQt4.QtGui import QMainWindow
from PilotInfo import PilotInfo
from PositionsModel import PositionsModel
from Remote import Scheduler
from Ui_RaceInfoWindow import Ui_RaceInfoWindow


class RaceInfoWindow(QMainWindow, Ui_RaceInfoWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        PilotInfo.init(self._refreshAllModels)
        self._switchButtonToStart()
        self.__lapTimeRecord = None
        self.__speedRecord = None
        self.__posModel = PositionsModel()
        self.positionsView.setModel(self.__posModel)
        handlers = {('init', 'best_lap'): self._newBestLap,
                    ('init', 'speed_record'): self._newBestSpeed}
        OTPApplication.registerMsgHandlers(handlers)

    def _newBestLap(self, msg):
        self.__lapTimeRecord = msg
        self._updateBestLapLabel(*msg)

    def _updateBestLapLabel(self, car, lap, time):
        self.bestLapLabel.setText("%s (%s in lap %i)" % (Util.secondsToString(time),
                                                         PilotInfo.get(car).name(),
                                                         lap))

    def _newBestSpeed(self, msg):
        self.__speedRecord = msg
        self._updateBestSpeedLabel(*msg)

    def _updateBestSpeedLabel(self, car, intermediate, lap, speed):
        s = "%s (%s in intermediate %i of lap %i)" % (Util.mpsToString(speed),
                                                      PilotInfo.get(car).name(),
                                                      intermediate,
                                                      lap)
        self.bestSpeedLabel.setText(s)

    def _refreshAllModels(self):
        if self.__lapTimeRecord is not None:
            self._updateBestLapLabel(*self.__lapTimeRecord)
        if self.__speedRecord is not None:
            self._updateBestSpeedLabel(*self.__speedRecord)
        self.__posModel.reset()

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
