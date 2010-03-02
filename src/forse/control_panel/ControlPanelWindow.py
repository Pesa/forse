from PyQt4.QtCore import QTimer, pyqtSlot
from PyQt4.QtGui import QApplication, QMainWindow, QMessageBox
from ConfigDialog import ConfigDialog
from Ui_ControlPanelWindow import Ui_ControlPanelWindow
from remote import BootstrapServer


class ControlPanelWindow(QMainWindow, Ui_ControlPanelWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        QApplication.instance().lastWindowClosed.connect(self._shutdown)
        QTimer.singleShot(0, self._startup)

    @pyqtSlot(name="on_bootstrapButton_clicked")
    def _bootstrap(self):
        BootstrapServer.bootstrap(self._bootstrapDone, *self._bootstrapArgs)

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

    def _shutdown(self):
        BootstrapServer.stop(lambda _: QApplication.quit())

    def _startup(self):
        if BootstrapServer.start():
            self.actionNewSimulation.trigger()
        else:
            QMessageBox.critical(self, "Fatal error", "Failed to start an instance of bootstrap_server.")
            QApplication.quit()
