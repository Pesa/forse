from PyQt4.Qt import Qt
from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QIcon, QMainWindow
from Subscriber import SubscriberApplication
from PilotInfo import PilotInfo
from Remote import Scheduler
from PositionsDock import PositionsDock
from TelemetryDock import TelemetryDock
from Util import mpsToString, secondsToString
from Ui_RaceInfoWindow import Ui_RaceInfoWindow


class RaceInfoWindow(QMainWindow, Ui_RaceInfoWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self.positionsDock = PositionsDock(self)
        self.telemetryDock = TelemetryDock(self)
        self.addDockWidget(Qt.RightDockWidgetArea, self.positionsDock)
        self.addDockWidget(Qt.RightDockWidgetArea, self.telemetryDock)
        self.__playIcon = QIcon(":/icons/play.png")
        self.__pauseIcon = QIcon(":/icons/pause.png")
        self.__lapTimeRecord = None
        self.__speedRecord = None
        PilotInfo.init(self.reloadPilotInfo)
        handlers = {('init', 'best_lap'): self._newBestLap,
                    ('init', 'speed_record'): self._newBestSpeed,
                    ('init', 'race_state'): self._setRaceState,
                    ('init', 'speedup'): self._setSpeedup}
        SubscriberApplication.registerMsgHandlers(handlers)
        SubscriberApplication.instance().subscribed.connect(self.statusBar().clearMessage)
        SubscriberApplication.instance().subscriptionError.connect(self._subscriptionError)
        SubscriberApplication.instance().subscribe()

    @pyqtSlot(name="on_speedupSlider_sliderReleased")
    def changeSpeedup(self):
        Scheduler.setSpeedup(self._checkReply, self.speedupSlider.value())

    def pauseSimulation(self):
        self.startpauseButton.setEnabled(False)
        Scheduler.pauseSimulation(self._checkReply)

    def startSimulation(self):
        self.startpauseButton.setEnabled(False)
        Scheduler.startSimulation(self._checkReply)

    def reloadPilotInfo(self):
        if self.__lapTimeRecord is not None:
            self._updateBestLapLabel(*self.__lapTimeRecord)
        if self.__speedRecord is not None:
            self._updateBestSpeedLabel(*self.__speedRecord)
        self.positionsDock.reloadPilotInfo()
        self.telemetryDock.reloadPilotInfo()
        self.raceView.reloadPilotInfo()

    def _checkReply(self, reply):
        if reply != "ok":
            self.statusBar().showMessage(str(reply), 5000)

    def _newBestLap(self, msg):
        self.__lapTimeRecord = msg
        self._updateBestLapLabel(*msg)

    def _updateBestLapLabel(self, car, lap, time):
        self.bestLapLabel.setText("%s (%s in lap %i)" % (secondsToString(time),
                                                         PilotInfo.get(car).name(),
                                                         lap))

    def _newBestSpeed(self, msg):
        self.__speedRecord = msg
        self._updateBestSpeedLabel(*msg)

    def _updateBestSpeedLabel(self, car, intermediate, lap, speed):
        s = "%s (%s in intermediate %i of lap %i)" % (mpsToString(speed),
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
            self.statusBar().showMessage("Unknown race_state: " + state.text, 5000)

    def _setSpeedup(self, speedup):
        if isinstance(speedup, int):
            self.speedupSlider.setValue(speedup)
            self.speedupSlider.setEnabled(True)
        else:
            self.speedupSlider.setEnabled(False)

    @pyqtSlot(int, name="on_speedupSlider_valueChanged")
    def _speedupSliderChanged(self, value):
        self.speedupLabel.setText("%i%%" % value)

    def _subscriptionError(self):
        self.statusBar().showMessage("Subscription failed, retrying ...", 1500)
