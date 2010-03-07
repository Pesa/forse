from PyQt4.QtCore import QTimer, pyqtSlot
from PyQt4.QtGui import QMainWindow, QMessageBox
from ConfigDialog import ConfigDialog
from Ui_ControlPanelWindow import Ui_ControlPanelWindow
from remote import BootstrapServer
from util import NodeApplication


class ControlPanelWindow(QMainWindow, Ui_ControlPanelWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        NodeApplication.instance().lastWindowClosed.connect(self._shutdown)
        BootstrapServer.ready.connect(self._ready)
        BootstrapServer.notReady.connect(self._notReady)
        QTimer.singleShot(0, self._startup)

    @pyqtSlot(name="on_bootstrapButton_clicked")
    def _bootstrap(self):
        BootstrapServer.bootstrap(self._bootstrapDone, *self._bootstrapArgs)

    def _bootstrapDone(self, reply):
        if reply == "ok":
            self.statusBar.showMessage("System bootstrapped successfully", 5000)
            self.actionNewSimulation.setEnabled(False)
        else:
            self.statusBar.showMessage("Bootstrap error", 5000)
            QMessageBox.critical(self, "Error", "An error occurred during bootstrap:\n\n   %s" % reply)

    @pyqtSlot(name="on_actionNewSimulation_triggered")
    def _newSimulation(self):
        dialog = ConfigDialog(self)
        if dialog.exec_() == ConfigDialog.Accepted:
            self._bootstrapArgs = dialog.bootstrapArgs()

    def _notReady(self):
        self.bootstrapButton.setEnabled(False)

    def _ready(self):
        self.bootstrapButton.setEnabled(True)

    def _shutdown(self):
        BootstrapServer.stop(lambda _: NodeApplication.quit())

    def _startup(self):
        if BootstrapServer.start():
            BootstrapServer.setGuiNode(None, NodeApplication.instance().nodeName())
            self.actionNewSimulation.trigger()
        else:
            QMessageBox.critical(self, "Fatal error", "Failed to start an instance of bootstrap_server.")
            NodeApplication.quit()
