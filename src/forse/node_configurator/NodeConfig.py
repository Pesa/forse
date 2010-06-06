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

from twotp import Atom
from PyQt4.QtCore import QTimer, pyqtSlot
from PyQt4.QtGui import QDialog, QMessageBox
from Remote import NodeManager
from Ui_NodeConfig import Ui_NodeConfig


class NodeConfig(QDialog, Ui_NodeConfig):

    def __init__(self, parent=None):
        QDialog.__init__(self, parent)
        self.setupUi(self)
        QTimer.singleShot(0, self._startup)

    @pyqtSlot(name="on_buttons_accepted")
    def _configure(self):
        self.buttons.setEnabled(False)
        args = [(Atom("event_dispatcher"), self.dispatcherSpinBox.value()),
                (Atom("scheduler"), self.schedulerSpinBox.value()),
                (Atom("car"), self.carSpinBox.value()),
                (Atom("team"), self.teamSpinBox.value()),
                (Atom("weather"), self.weatherSpinBox.value())]
        NodeManager.configure(self._configureDone, args)

    def _configureDone(self, reply):
        if reply == "ok":
            self.accept()
        else:
            QMessageBox.critical(self, "Error", "Communication or configuration error:\n\n   %s" % reply)
            self.buttons.setEnabled(True)

    def _startup(self):
        if not NodeManager.start():
            QMessageBox.critical(self, "Fatal error", "Failed to start an instance of node_manager.")
            self.reject()
