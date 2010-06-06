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

from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QWidget, QMessageBox
from OTPApplication import OTPApplication
from Remote import Car
from SpeedModel import SpeedModel
from TimeModel import TimeModel
from Ui_CarStatusWidget import Ui_CarStatusWidget


class CarStatusWidget(QWidget, Ui_CarStatusWidget):

    def __init__(self, carId, status, pitCount):
        QWidget.__init__(self)
        self.setupUi(self)
        self.__fuel = None
        self.__id = carId
        self.statusLabel.setText(status.text)
        if status.text == "retired":
            self.pitstopButton.setEnabled(False)
            self.retireButton.setEnabled(False)
        self.psCountLabel.setText(str(pitCount))
        self.speedView.setModel(SpeedModel(self.__id))
        self.timeView.setModel(TimeModel(self.__id))
        handlers = {('init', 'consumption'): self._setConsumption,
                    ('init', 'max_fuel'): self._setMaxFuel,
                    ('init', 'pitstop'): self._newPitstop,
                    ('init', 'retire'): self._setRetired}
        OTPApplication.registerMsgHandlers(handlers)

    @pyqtSlot(name="on_pitstopButton_clicked")
    def forcePitstop(self):
        self.pitstopButton.setEnabled(False)
        Car.forcePitstop(self._forcePitstopDone, self.__id)

    @pyqtSlot(name="on_retireButton_clicked")
    def retire(self):
        self.retireButton.setEnabled(False)
        Car.retire(self._retireDone, self.__id)

    def _forcePitstopDone(self, reply):
        if reply != "ok":
            QMessageBox.warning(self, "Error", "Failed to force a pitstop:\n\n   %s" % reply)
        self.pitstopButton.setEnabled(True)

    def _newPitstop(self, carId, pitCount, fuelAdded, newTyres):
        if carId == self.__id:
            self.psCountLabel.setText(str(pitCount))
            if fuelAdded > 0:
                s = "+%i liters of fuel" % round(fuelAdded)
            else:
                s = "no refueling"
            s += "; new set of %s tyres" % newTyres.text
            self.psOpsLabel.setText(s)
            self.__fuel = min(self.__fuel + fuelAdded, self.fuelBar.maximum())
            self.fuelBar.setValue(round(self.__fuel))
            self.tyresBar.setValue(100)
            self.tyresLabel.setText(newTyres.text)

    def _retireDone(self, reply):
        if reply != "ok":
            QMessageBox.warning(self, "Error", "Car retirement failed:\n\n   %s" % reply)

    def _setConsumption(self, carId, _interm, _lap, fuel, tyresCons, tyresType):
        if carId == self.__id:
            self.__fuel = fuel
            self.fuelBar.setValue(round(self.__fuel))
            self.tyresBar.setValue(round(100 - tyresCons))
            self.tyresLabel.setText(tyresType.text)

    def _setMaxFuel(self, maxFuel):
        self.fuelBar.setMaximum(maxFuel)
        if self.__fuel is not None:
            self.fuelBar.setValue(round(self.__fuel))

    def _setRetired(self, carId):
        if carId == self.__id:
            self.statusLabel.setText("retired")
            self.pitstopButton.setEnabled(False)
            self.retireButton.setEnabled(False)
