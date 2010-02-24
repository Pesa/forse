from PyQt4.QtCore import QTimer, pyqtSlot
from PyQt4.QtGui import QMainWindow, QMessageBox
from ConfigDialog import ConfigDialog
from Ui_ControlPanelWindow import Ui_ControlPanelWindow
from util import RPC


class ControlPanelWindow(QMainWindow, Ui_ControlPanelWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self._bootstrapRPC = RPC("bootstrap_server", "bootstrap")
        self._bootstrapRPC.done.connect(self._bootstrapDone)
        QTimer.singleShot(0, self.actionNewSimulation.trigger)

    @pyqtSlot(name="on_bootstrapButton_clicked")
    def _bootstrap(self):
        self._bootstrapRPC.call(*self._bootstrapArgs)

    def _bootstrapDone(self, reply):
        if reply == "ok":
            self.statusBar.showMessage("System bootstrapped successfully", 10000)
            self.actionNewSimulation.setEnabled(False)
        else:
            self.statusBar.showMessage("Bootstrap error", 10000)
            QMessageBox.critical(self, "Error", "An error occurred during bootstrap:\n\n   %s" % reply)

    @pyqtSlot(name="on_actionNewSimulation_triggered")
    def _newSimulation(self):
        dialog = ConfigDialog(self)
        if dialog.exec_() == ConfigDialog.Accepted:
            self._bootstrapArgs = dialog.getBootstrapArgs()
            self.bootstrapButton.setEnabled(True)
