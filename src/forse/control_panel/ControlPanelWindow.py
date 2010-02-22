from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QMainWindow
from ConfigDialog import ConfigDialog
from Ui_ControlPanelWindow import Ui_ControlPanelWindow


class ControlPanelWindow(QMainWindow, Ui_ControlPanelWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self._configDialog = ConfigDialog(self)
        QTimer.singleShot(0, self._configDialog.exec_)
