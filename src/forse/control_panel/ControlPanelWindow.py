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
        self.__nodesList = []
        self.__nodesModel = QStringListModel(self)
        self.nodesView.setModel(self.__nodesModel)
        OTPApplication.instance().lastWindowClosed.connect(self._quit)
        BootstrapServer.nodeDown.connect(self._nodeDown)
        BootstrapServer.nodeUp.connect(self._nodeUp)
        BootstrapServer.ready.connect(lambda: self.actionBootstrap.setEnabled(True))
        BootstrapServer.notReady.connect(lambda: self.actionBootstrap.setEnabled(False))
        QTimer.singleShot(0, self._startup)

    @pyqtSlot(name="on_actionBootstrap_triggered")
    def _bootstrap(self):
        self.actionNew.setEnabled(False)
        self.actionBootstrap.setEnabled(False)
        BootstrapServer.bootstrap(self._bootstrapDone, *self.__bootstrapArgs)

    def _bootstrapDone(self, reply):
        if reply == "ok":
            self.statusBar().showMessage("System bootstrapped successfully", 5000)
            self.actionShutdown.setEnabled(True)
        else:
            self.statusBar().showMessage("Bootstrap failed", 5000)
            QMessageBox.critical(self, "Error", "An error occurred during bootstrap:\n\n   %s" % reply)

    @pyqtSlot(name="on_actionNew_triggered")
    def _newSimulation(self):
        dialog = ConfigDialog(self)
        if dialog.exec_() == ConfigDialog.Accepted:
            self.__bootstrapArgs = dialog.bootstrapArgs()

    def _nodeDown(self, node):
        try:
            self.__nodesList.remove(str(node))
            self.__nodesModel.setStringList(self.__nodesList)
        except ValueError:
            pass

    def _nodeUp(self, node):
        self.__nodesList.append(str(node))
        self.__nodesModel.setStringList(self.__nodesList)

    def _quit(self):
        BootstrapServer.shutdown(lambda _: OTPApplication.quit(), Atom("true"))

    def _setGuiNode(self):
        BootstrapServer.setGuiNode(self._setGuiNodeDone, OTPApplication.nodeName())

    def _setGuiNodeDone(self, reply):
        if reply == "ok":
            QTimer.singleShot(0, self.actionNew.trigger)
        else:
            QTimer.singleShot(500, self._setGuiNode)

    @pyqtSlot(name="on_actionShutdown_triggered")
    def _shutdown(self):
        self.actionShutdown.setEnabled(False)
        BootstrapServer.shutdown(self._shutdownDone, Atom("false"))

    def _shutdownDone(self, reply):
        if reply == "ok":
            self.statusBar().showMessage("System shutdown complete", 5000)
            self.actionNew.setEnabled(True)
        else:
            self.statusBar().showMessage("Shutdown failed", 5000)
            QMessageBox.critical(self, "Error", "An error occurred during shutdown:\n\n   %s" % reply)

    def _startup(self):
        if BootstrapServer.start():
            self._setGuiNode()
        else:
            QMessageBox.critical(self, "Fatal error", "Failed to start an instance of bootstrap_server.")
            OTPApplication.quit()
