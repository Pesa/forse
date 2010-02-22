from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QDialog
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
        self._submit = RPC("bootstrap_server", "read_config_files")
        self._submit.done.connect(self._foobar)

    def _foobar(self, reply):
        print reply

    @pyqtSlot(name="on_buttons_accepted")
    def _submitConfig(self):
        self._submit.call(self.teamsFileChooser.getFileName(),
                          self.trackFileChooser.getFileName(),
                          self.weatherFileChooser.getFileName())
