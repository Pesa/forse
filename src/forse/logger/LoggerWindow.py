import OTPApplication, Util
from PyQt4.QtGui import QMainWindow
from Ui_LoggerWindow import Ui_LoggerWindow


class LoggerWindow(QMainWindow, Ui_LoggerWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        handlers = {'init': self._setLogMsg,
                    'update': self._appendLogMsg}
        OTPApplication.registerMsgHandlers(handlers)

    def _appendLogMsg(self, msg):
        self.viewer.appendPlainText(Util.listToString(msg))

    def _setLogMsg(self, msg):
        self.viewer.setPlainText(Util.listToString(msg))
