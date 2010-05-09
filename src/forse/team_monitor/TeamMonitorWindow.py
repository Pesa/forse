from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QMainWindow
from Subscriber import SubscriberApplication
from CarStatusWidget import CarStatusWidget
from TeamChooser import TeamChooser
from Util import listToString
from Ui_TeamMonitorWindow import Ui_TeamMonitorWindow


class TeamMonitorWindow(QMainWindow, Ui_TeamMonitorWindow):

    def __init__(self):
        QMainWindow.__init__(self)
        self.setupUi(self)
        SubscriberApplication.instance().subscribed.connect(self.statusBar().clearMessage)
        SubscriberApplication.instance().subscriptionError.connect(self._subscriptionError)
        QTimer.singleShot(0, self._chooseTeam)

    def _chooseTeam(self):
        dialog = TeamChooser(self)
        SubscriberApplication.instance().subscribe()
        if dialog.exec_() == TeamChooser.Accepted:
            teamId, teamName = dialog.chosenTeam()
            self.setWindowTitle(teamName + ' ' + self.windowTitle())
            SubscriberApplication.removeAllHandlers()
            handlers = {('init', 'new_pilot'): self._newPilot}
            SubscriberApplication.registerMsgHandlers(handlers)
            SubscriberApplication.instance().setSubscriptionOptions([teamId])
            SubscriberApplication.instance().subscribe()
        else:
            SubscriberApplication.quit()

    def _newPilot(self, carId, name, status, pitCount):
        w = CarStatusWidget(carId, status, pitCount)
        self.tabWidget.addTab(w, listToString(name))

    def _subscriptionError(self):
        self.statusBar().showMessage("Subscription failed, retrying ...", 1500)
