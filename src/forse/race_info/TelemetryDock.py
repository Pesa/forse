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
from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QDockWidget
from TelemetryModel import TelemetryModel
from Ui_TelemetryDock import Ui_TelemetryDock


class TelemetryDock(QDockWidget, Ui_TelemetryDock):

    def __init__(self, parent=None):
        QDockWidget.__init__(self, parent)
        self.setupUi(self)
        self.__availableMap = {}
        self.__model = TelemetryModel()
        self.__model.modelReset.connect(self.telemetryView.resizeColumnsToContents, Qt.QueuedConnection)
        self.__model.newIntermediate.connect(self._addIntermediate)
        self.__model.newLap.connect(self._addLap)
        self.telemetryView.setModel(self.__model)

    def currentIntermediate(self):
        intIndex = self.intermediateComboBox.currentIndex()
        intermediate, ok = self.intermediateComboBox.itemData(intIndex).toInt()
        return intermediate if ok else None

    def currentLap(self):
        lapIndex = self.lapComboBox.currentIndex()
        lap, ok = self.lapComboBox.itemData(lapIndex).toInt()
        return lap if ok else None

    def reloadPilotInfo(self):
        self.telemetryView.reset()
        self.telemetryView.resizeColumnsToContents()

    def _addIntermediate(self, lap, intermediate):
        self.__availableMap[lap].append(intermediate)
        if self.currentLap() == lap:
            self.intermediateComboBox.addItem(str(intermediate), intermediate)
            if self.autoCheckBox.isChecked():
                self.intermediateComboBox.setCurrentIndex(self.intermediateComboBox.count() - 1)

    def _addLap(self, lap):
        self.__availableMap[lap] = []
        self.lapComboBox.addItem(str(lap), lap)
        if self.autoCheckBox.isChecked():
            self.lapComboBox.setCurrentIndex(self.lapComboBox.count() - 1)

    @pyqtSlot(int, name="on_intermediateComboBox_currentIndexChanged")
    def _intermediateChanged(self, _index):
        self.__model.setLapAndIntermediate(self.currentLap(), self.currentIntermediate())

    @pyqtSlot(int, name="on_lapComboBox_currentIndexChanged")
    def _lapChanged(self, _index):
        lap = self.currentLap()
        intermediate = self.currentIntermediate()
        self.intermediateComboBox.clear()
        if lap in self.__availableMap:
            for i in self.__availableMap[lap]:
                self.intermediateComboBox.addItem(str(i), i)
        newIndex = self.intermediateComboBox.findData(intermediate)
        self.intermediateComboBox.setCurrentIndex(newIndex)

    @pyqtSlot(bool, name="on_autoCheckBox_toggled")
    def _toggleAutoAdvance(self, checked):
        self.lapComboBox.setEnabled(not checked)
        self.intermediateComboBox.setEnabled(not checked)
        self.lapComboBox.setCurrentIndex(self.lapComboBox.count() - 1)
        self.intermediateComboBox.setCurrentIndex(self.intermediateComboBox.count() - 1)
