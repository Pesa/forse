import sys
from PyQt4.QtGui import QMainWindow
from Ui_MainWindow import Ui_MainWindow
from util import NodeApplication


class MainWindow(QMainWindow, Ui_MainWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        # FIXME: improve handlers API
        handlers = {('init', ''): self._setLogMsg,
                    ('update', ''): self._appendLogMsg}
        NodeApplication.instance().registerMsgHandlers(handlers)

    def _appendLogMsg(self, msg):
        self.viewer.appendPlainText(self.__printable(msg))

    def _setLogMsg(self, msg):
        self.viewer.setPlainText(self.__printable(msg))

    def __printable(self, text):
        return ''.join([ chr(x) for x in text ])


if __name__ == "__main__":
    app = NodeApplication("debug_log")
    mainwin = MainWindow()
    mainwin.show()
    sys.exit(app.exec_())
