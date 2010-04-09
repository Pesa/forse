from PyQt4.Qt import Qt
from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QInputDialog, QMainWindow, QSplitter
from Subscriber import SubscriberApplication
from CarStatusWidget import CarStatusWidget
from Ui_TeamMonitorWindow import Ui_TeamMonitorWindow


class TeamMonitorWindow(QMainWindow, Ui_TeamMonitorWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        self.splitter = QSplitter(Qt.Vertical, self)
        self.horizontalLayout.addWidget(self.splitter)
        handlers = {('init', 'new_pilot'): self._newPilot}
        SubscriberApplication.registerMsgHandlers(handlers)
        QTimer.singleShot(0, self._chooseTeam)

    def _chooseTeam(self):
        teamId, ok = QInputDialog.getInt(self, "Choose team", "Choose a team to monitor:", 1, 1)
        if ok:
            SubscriberApplication.instance().setSubscriptionOptions([teamId])
            SubscriberApplication.instance().subscribe()
        else:
            SubscriberApplication.quit()

    def _newPilot(self, carId, name, status, pitCount):
        w = CarStatusWidget(self.splitter, carId, name, status, pitCount)
        self.splitter.addWidget(w)
