###########################################################################
#
# Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
#
# This file is part of FORSE.
#
# FORSE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# FORSE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
#
###########################################################################

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

    def _newPilot(self, carId, name, state, pitCount):
        w = CarStatusWidget(carId, state, pitCount)
        self.tabWidget.addTab(w, listToString(name))

    def _subscriptionError(self):
        self.statusBar().showMessage("Subscription failed, retrying ...", 1500)
