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

from PyQt4.Qt import Qt
from PyQt4.QtGui import QDockWidget
from PositionsModel import PositionsModel
from Ui_PositionsDock import Ui_PositionsDock


class PositionsDock(QDockWidget, Ui_PositionsDock):

    def __init__(self, parent=None):
        QDockWidget.__init__(self, parent)
        self.setupUi(self)
        self.__model = PositionsModel()
        self.__model.modelReset.connect(self.positionsView.resizeColumnsToContents, Qt.QueuedConnection)
        self.positionsView.setModel(self.__model)

    def reloadPilotInfo(self):
        self.positionsView.reset()
        self.positionsView.resizeColumnsToContents()
