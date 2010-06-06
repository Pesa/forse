###########################################################################
#
# Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
#
# This file is part of FORSE.
#
# FORSE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# FORSE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
#
###########################################################################

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
        self.trackFileChooser.setDefaultPath("examples/monza.conf")
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
