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
        ### FIXME: temporary stuff, remove me!! ###
        self.dispatcherSpinBox.setValue(1)
        self.schedulerSpinBox.setValue(1)
        self.carSpinBox.setValue(10)
        self.teamSpinBox.setValue(5)
        self.weatherSpinBox.setValue(1)
        ######

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
