import sys, twotp
from twotp.term import Atom
from PyQt4 import QtCore, QtGui
from Ui_MainWindow import Ui_MainWindow


class NotificationHandler(object):

	def __init__(self, process, viewer):
		object.__init__(self)
		self._process = process
		self._viewer = viewer

	def printable(self, text):
		return ''.join([ chr(x) for x in text ])

	def remote_handle(self, type, msg):
		if type.text == "init":
			self._viewer.setPlainText(self.printable(msg))
		elif type.text == "update":
			self._viewer.appendPlainText(self.printable(msg))


class MainWindow(QtGui.QMainWindow, Ui_MainWindow):

	def __init__(self):
		QtGui.QMainWindow.__init__(self)
		self.setupUi(self)
		self._cookie = twotp.readCookie()
		self._nodeName = twotp.buildNodeName("pyfoo")

	@QtCore.pyqtSlot(name="on_actionConnect_triggered")
	def connect(self):
		self.actionConnect.setEnabled(False)

		def errorCB(error):
			self.statusBar.showMessage("Failed to connect: " + str(error))
			self.actionConnect.setEnabled(True)

		def subscribeCB(result):
			if isinstance(result, Atom) and result.text == "ok":
				msg = "Connected"
			else:
				msg = "Subscription failed"
				self.actionConnect.setEnabled(True)
			self.statusBar.showMessage(msg)

		def nodeCB(result):
			self._process.register("dbg_listener")
			self._process.registerModule("dbg_listener", NotificationHandler(self._process, self.viewer))
			self._process.listen()
			args = [self._process.pid.nodeName, Atom("dbg_listener"), Atom("handle")]
			callback = Atom("callback"), Atom("rpc"), Atom("call"), args
			self._process.callRemote(result.text, "event_dispatcher", "subscribe", Atom("debug_log"),
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
	mainwin = MainWindow()
	mainwin.show()
	sys.exit(app.exec_())
