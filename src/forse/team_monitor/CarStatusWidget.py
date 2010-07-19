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

    def __init__(self, carId, state, pitCount, config):
        QWidget.__init__(self)
        self.setupUi(self)
        self.__config = config
        self.__fuel = None
        self.__id = carId
        self._setCarState(carId, state)
        self.psCountLabel.setText(str(pitCount))
        self.speedView.setModel(SpeedModel(self.__id))
        self.timeView.setModel(TimeModel(self.__id))
        handlers = {('init', 'car_state'): self._setCarState,
                    ('init', 'consumption'): self._setConsumption,
                    ('init', 'max_fuel'): self._setMaxFuel,
                    ('init', 'pitstop'): self._newPitstop}
        OTPApplication.registerMsgHandlers(handlers)

    @pyqtSlot(name="on_configButton_clicked")
    def configDetails(self):
        s = "Car's weight\t\t %(car_weight)s kg\n" \
            "Engine's power\t %(power)s\n" \
            "Brakes' efficiency\t %(brake)s\n" \
            "\n" \
            "Pilot's weight\t %(pilot_weight)s kg\n" \
            "Pilot's skill\t\t %(skill)i\n" \
            % self.__config
        QMessageBox.information(self, "Configuration details", s)

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

    def _setCarState(self, carId, state):
        if carId == self.__id:
            if isinstance(state, tuple):
                state, reason = state
                s = "%s (%s)" % (state.text, reason.text)
            else:
                s = state.text
            self.stateLabel.setText(s)
            if state.text == "ended" or state.text == "retired":
                self.pitstopButton.setEnabled(False)
                self.retireButton.setEnabled(False)

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
