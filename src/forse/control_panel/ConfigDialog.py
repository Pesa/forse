from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QDialog, QMessageBox
from Ui_ConfigDialog import Ui_ConfigDialog
from util import RPC


class ConfigDialog(QDialog, Ui_ConfigDialog):

    def __init__(self, parent=None):
        QDialog.__init__(self, parent)
        self.setupUi(self)
        self.teamsFileChooser.setLabel("Teams")
        self.teamsFileChooser.setDefaultPath("examples/teams.conf")
        self.trackFileChooser.setLabel("Track")
        self.trackFileChooser.setDefaultPath("examples/track.conf")
        self.weatherFileChooser.setLabel("Weather")
        self.weatherFileChooser.setDefaultPath("examples/weather.conf")
        self._configRPC = RPC("bootstrap_server", "read_config_files")
        self._configRPC.done.connect(self._configDone)

    def getBootstrapArgs(self):
        return self.lapsSpinBox.value(), self.speedupSpinBox.value()

    @pyqtSlot(name="on_buttons_accepted")
    def _submitConfig(self):
        self.setEnabled(False)
        self._configRPC.call(self.teamsFileChooser.getFileName(),
                             self.trackFileChooser.getFileName(),
                             self.weatherFileChooser.getFileName())

    def _configDone(self, reply):
        if reply == "ok":
            self.accept()
        else:
            QMessageBox.critical(self, "Error", "Configuration error:\n\n   %s" % reply)
            self.setEnabled(True)
