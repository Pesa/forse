from PyQt4.QtCore import QTimer, pyqtSlot
from PyQt4.QtGui import QMainWindow
from ConfigDialog import ConfigDialog
from Ui_ControlPanelWindow import Ui_ControlPanelWindow


class ControlPanelWindow(QMainWindow, Ui_ControlPanelWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        QTimer.singleShot(0, self.actionNewSimulation.trigger)

    @pyqtSlot(name="on_actionNewSimulation_triggered")
    def _newSimulation(self):
        d = ConfigDialog(self)
        d.exec_()
