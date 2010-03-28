import OTPApplication, Util
from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QIcon, QMainWindow
from PilotInfo import PilotInfo
from PositionsModel import PositionsModel
from Remote import Scheduler
from Ui_RaceInfoWindow import Ui_RaceInfoWindow


class RaceInfoWindow(QMainWindow, Ui_RaceInfoWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self.__playIcon = QIcon(":/icons/play.png")
        self.__pauseIcon = QIcon(":/icons/pause.png")
        self.__lapTimeRecord = None
        self.__speedRecord = None
        self.__speedup = None
        self.__posModel = PositionsModel()
        self.positionsView.setModel(self.__posModel)
        PilotInfo.init(self.refreshData)
        handlers = {('init', 'best_lap'): self._newBestLap,
                    ('init', 'speed_record'): self._newBestSpeed,
                    ('init', 'race_state'): self._setRaceState,
                    ('init', 'speedup'): self._setSpeedup}
        OTPApplication.registerMsgHandlers(handlers)

    @pyqtSlot(name="on_speedupApplyButton_clicked")
    def applySpeedup(self):
        self.speedupApplyButton.setEnabled(False)
        self.speedupSpinBox.setEnabled(False)
        Scheduler.setSpeedup(self._checkReply, self.speedupSpinBox.value())

    def pauseSimulation(self):
        self.startpauseButton.setEnabled(False)
        Scheduler.pauseSimulation(self._checkReply)

    def startSimulation(self):
        self.startpauseButton.setEnabled(False)
        Scheduler.startSimulation(self._checkReply)

    def refreshData(self):
        if self.__lapTimeRecord is not None:
            self._updateBestLapLabel(*self.__lapTimeRecord)
        if self.__speedRecord is not None:
            self._updateBestSpeedLabel(*self.__speedRecord)
        self.__posModel.reset()

    def _checkReply(self, reply):
        if reply != "ok":
            self.statusBar.showMessage(str(reply), 5000)

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

    def _setRaceState(self, state):
        self.simulationState.setText(state.text.upper())
        try:
            self.startpauseButton.clicked.disconnect()
        except TypeError:
            pass
        if state.text == "initialized":
            self.startpauseButton.clicked.connect(self.startSimulation)
            self.startpauseButton.setText("Start")
            self.startpauseButton.setIcon(self.__playIcon)
            self.startpauseButton.setEnabled(True)
        elif state.text == "running":
            self.startpauseButton.clicked.connect(self.pauseSimulation)
            self.startpauseButton.setText("Pause")
            self.startpauseButton.setIcon(self.__pauseIcon)
            self.startpauseButton.setEnabled(True)
        elif state.text == "paused":
            self.startpauseButton.clicked.connect(self.startSimulation)
            self.startpauseButton.setText("Resume")
            self.startpauseButton.setIcon(self.__playIcon)
            self.startpauseButton.setEnabled(True)
        elif state.text == "finished" or state.text == "terminated":
            self.startpauseButton.setEnabled(False)
        else:
            self.statusBar.showMessage("Unknown race_state: " + state.text, 5000)

    def _setSpeedup(self, speedup):
        self.speedupApplyButton.setEnabled(False)
        if isinstance(speedup, int):
            self.__speedup = speedup
            self.speedupSpinBox.setValue(speedup)
            self.speedupSpinBox.setEnabled(True)
        else:
            self.speedupSpinBox.setEnabled(False)

    @pyqtSlot(int, name="on_speedupSpinBox_valueChanged")
    def _speedupSpinBoxChanged(self, newValue):
        if newValue == self.__speedup:
            self.speedupApplyButton.setEnabled(False)
        else:
            self.speedupApplyButton.setEnabled(True)
