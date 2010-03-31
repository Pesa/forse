from PyQt4.QtGui import QDockWidget
from TelemetryModel import TelemetryModel
from Ui_TelemetryDock import Ui_TelemetryDock


class TelemetryDock(QDockWidget, Ui_TelemetryDock):

    def __init__(self, parent=None):
        QDockWidget.__init__(self, parent)
        self.setupUi(self)
        self.__model = TelemetryModel()
        self.telemetryView.setModel(self.__model)

    def reloadPilotInfo(self):
        self.telemetryView.reset()
