from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QDialog, QMessageBox
from Remote import BootstrapServer
from Ui_ConfigDialog import Ui_ConfigDialog


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

    def bootstrapArgs(self):
        return self.lapsSpinBox.value(), self.speedupSpinBox.value()

    @pyqtSlot(name="on_buttons_accepted")
    def _config(self):
        self.setEnabled(False)
        BootstrapServer.readConfigFiles(self._configDone,
                                        self.teamsFileChooser.fileName(),
                                        self.trackFileChooser.fileName(),
                                        self.weatherFileChooser.fileName())

    def _configDone(self, reply):
        if reply == "ok":
            self.accept()
        else:
            QMessageBox.warning(self, "Error", "Configuration error:\n\n   %s" % reply)
            self.setEnabled(True)
