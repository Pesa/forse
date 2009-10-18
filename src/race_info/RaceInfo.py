import sys
from PyQt4 import QtGui
from GraphicTrack import TrackWidget
from Ui_RaceInfoWindow import Ui_RaceInfoWindow


class RaceInfoWindow(QtGui.QMainWindow, Ui_RaceInfoWindow):

	def __init__(self):
		QtGui.QMainWindow.__init__(self)
		self.setupUi(self)
		self.setCentralWidget(TrackWidget(self))


if __name__ == "__main__":
	app = QtGui.QApplication(sys.argv)
	mainwin = RaceInfoWindow()
	mainwin.show()
	sys.exit(app.exec_())
