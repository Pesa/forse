import sys
from PyQt4.QtGui import QMainWindow
from Ui_RaceInfoWindow import Ui_RaceInfoWindow
from util import NodeApplication


class RaceInfoWindow(QMainWindow, Ui_RaceInfoWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)


if __name__ == "__main__":
    app = NodeApplication("race_info")
    mainwin = RaceInfoWindow()
    mainwin.show()
    sys.exit(app.exec_())
