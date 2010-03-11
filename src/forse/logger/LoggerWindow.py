from PyQt4.QtGui import QMainWindow
from Ui_LoggerWindow import Ui_LoggerWindow
from util import listToString, NodeApplication


class LoggerWindow(QMainWindow, Ui_LoggerWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        # FIXME: improve handlers API
        handlers = {('init', ''): self._setLogMsg,
                    ('update', ''): self._appendLogMsg}
        NodeApplication.instance().registerMsgHandlers(handlers)

    def _appendLogMsg(self, msg):
        self.viewer.appendPlainText(listToString(msg))

    def _setLogMsg(self, msg):
        self.viewer.setPlainText(listToString(msg))
