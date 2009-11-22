import sys, twotp
from twotp.term import Atom
from PyQt4.QtCore import QTimer
from PyQt4 import QtGui
from TrackWidget import TrackWidget
from Ui_RaceInfoWindow import Ui_RaceInfoWindow


class RaceInfoWindow(QtGui.QMainWindow, Ui_RaceInfoWindow):

	def __init__(self):
		QtGui.QMainWindow.__init__(self)
		self.setupUi(self)
		self.setCentralWidget(TrackWidget(self))
		self._cookie = twotp.readCookie()
		self._nodeName = twotp.buildNodeName("pyinfo")
		QTimer.singleShot(0, self.connect)

	def connect(self):

		def errorCB(error):
			self.statusBar.showMessage("Failed to connect: " + str(error))

		def subscribeCB(result):
			if isinstance(result, Atom) and result.text == "ok":
				msg = "Connected"
			else:
				msg = "Subscription failed"
			self.statusBar.showMessage(msg)

		def nodeCB(result):
			self._process.register("race_listener")
			self._process.registerModule("race_listener", self.centralWidget())
			self._process.listen()
			args = [self._process.pid.nodeName, Atom("race_listener"), Atom("handle")]
			callback = Atom("callback"), Atom("rpc"), Atom("call"), args
			self._process.callRemote(result.text, "event_dispatcher", "subscribe", Atom("race_info"),
									callback).addCallback(subscribeCB).addErrback(errorCB)

		def resolveCB(result):
			self._process.callRemote("foo", "erlang", "node", result).addCallback(nodeCB)

		self._process = twotp.Process(self._nodeName, self._cookie)
		self._process.callRemote("foo", "global", "whereis_name", Atom("event_dispatcher")
								).addCallback(resolveCB).addErrback(errorCB)
		from twisted.internet import reactor
		reactor.run()


if __name__ == "__main__":
	app = QtGui.QApplication(sys.argv)
	import qt4reactor
	qt4reactor.install()
	mainwin = RaceInfoWindow()
	mainwin.show()
	sys.exit(app.exec_())
