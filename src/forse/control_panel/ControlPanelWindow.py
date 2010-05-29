from twotp import Atom
from PyQt4.QtCore import QTimer, pyqtSlot
from PyQt4.QtGui import QMainWindow, QMessageBox, QStringListModel
from ConfigDialog import ConfigDialog
from OTPApplication import OTPApplication
from Remote import BootstrapServer
from Ui_ControlPanelWindow import Ui_ControlPanelWindow


class ControlPanelWindow(QMainWindow, Ui_ControlPanelWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self.nodesList = []
        self.nodesModel = QStringListModel(self.nodesList, self)
        self.nodesView.setModel(self.nodesModel)
        OTPApplication.instance().lastWindowClosed.connect(self._shutdown)
        BootstrapServer.nodeDown.connect(self._nodeDown)
        BootstrapServer.nodeUp.connect(self._nodeUp)
        BootstrapServer.ready.connect(lambda: self.bootstrapButton.setEnabled(True))
        BootstrapServer.notReady.connect(lambda: self.bootstrapButton.setEnabled(False))
        QTimer.singleShot(0, self._startup)

    @pyqtSlot(name="on_bootstrapButton_clicked")
    def _bootstrap(self):
        BootstrapServer.bootstrap(self._bootstrapDone, *self._bootstrapArgs)

    def _bootstrapDone(self, reply):
        if reply == "ok":
            self.statusBar().showMessage("System bootstrapped successfully", 5000)
            self.actionNewSimulation.setEnabled(False)
            self.bootstrapButton.setEnabled(False)
        else:
            self.statusBar().showMessage("Bootstrap error", 5000)
            QMessageBox.critical(self, "Error", "An error occurred during bootstrap:\n\n   %s" % reply)

    @pyqtSlot(name="on_actionNewSimulation_triggered")
    def _newSimulation(self):
        dialog = ConfigDialog(self)
        if dialog.exec_() == ConfigDialog.Accepted:
            self._bootstrapArgs = dialog.bootstrapArgs()

    def _nodeDown(self, node):
        try:
            self.nodesList.remove(str(node))
            self.nodesModel.setStringList(self.nodesList)
        except ValueError:
            pass

    def _nodeUp(self, node):
        self.nodesList.append(str(node))
        self.nodesModel.setStringList(self.nodesList)

    def _setGuiNode(self):
        BootstrapServer.setGuiNode(self._setGuiNodeDone, OTPApplication.nodeName())

    def _setGuiNodeDone(self, reply):
        if reply == "ok":
            QTimer.singleShot(0, self.actionNewSimulation.trigger)
        else:
            QTimer.singleShot(500, self._setGuiNode)

    def _shutdown(self):
        BootstrapServer.shutdown(lambda _: OTPApplication.quit(), Atom("true"))

    def _startup(self):
        if BootstrapServer.start():
            self._setGuiNode()
        else:
            QMessageBox.critical(self, "Fatal error", "Failed to start an instance of bootstrap_server.")
            OTPApplication.quit()
