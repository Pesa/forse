from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QStackedWidget
from Ui_ControlPanel import Ui_ControlPanel
from util import NodeApplication


class ControlPanel(QStackedWidget, Ui_ControlPanel):

    def __init__(self):
        QStackedWidget.__init__(self)
        self.setupUi(self)
        self.teamsFileChooser.setLabel("Teams")
        self.teamsFileChooser.setDefaultPath("examples/teams.conf")
        self.trackFileChooser.setLabel("Track")
        self.trackFileChooser.setDefaultPath("examples/track.conf")
        self.weatherFileChooser.setLabel("Weather")
        self.weatherFileChooser.setDefaultPath("examples/weather.conf")

    @pyqtSlot(name="on_confirmButton_clicked")
    def _submitConfig(self):
        d = NodeApplication.instance().rpc("bootstrap_server", "read_config_files",
                                           self.teamsFileChooser.getFileName(),
                                           self.trackFileChooser.getFileName(),
                                           self.weatherFileChooser.getFileName())
        #self.setCurrentWidget(self.statusPage)