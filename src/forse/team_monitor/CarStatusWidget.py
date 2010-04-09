from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QGroupBox, QMessageBox
from OTPApplication import OTPApplication
from Remote import Car
from Util import listToString
from Ui_CarStatusWidget import Ui_CarStatusWidget


class CarStatusWidget(QGroupBox, Ui_CarStatusWidget):

    def __init__(self, parent, carId, pilotName, status, pitCount):
        QGroupBox.__init__(self, parent)
        self.setupUi(self)
        self._fuel = None
        self._id = carId
        self.setTitle(listToString(pilotName))
        self.psCountLabel.setText(str(pitCount))
        if status.text == "retired":
            self._setRetired(self._id)
        handlers = {('init', 'consumption'): self._setConsumption,
                    ('init', 'max_fuel'): self._setMaxFuel,
                    ('init', 'pitstop'): self._newPitstop,
                    ('init', 'retire'): self._setRetired}
        OTPApplication.registerMsgHandlers(handlers)

    @pyqtSlot(name="on_pitstopButton_clicked")
    def forcePitstop(self):
        self.pitstopButton.setEnabled(False)
        Car.forcePitstop(self._forcePitstopDone, self._id)

    @pyqtSlot(name="on_retireButton_clicked")
    def retire(self):
        self.retireButton.setEnabled(False)
        Car.retire(self._retireDone, self._id)

    def _forcePitstopDone(self, reply):
        if reply != "ok":
            QMessageBox.warning(self, "Error", "Failed to force a pitstop:\n\n   %s" % reply)
        self.pitstopButton.setEnabled(True)

    def _newPitstop(self, carId, pitCount, fuelAdded, newTyres):
        if carId == self._id:
            self.psCountLabel.setText(str(pitCount))
            self._fuel += fuelAdded
            self.fuelBar.setValue(round(self._fuel))
            self.tyresBar.setValue(100)
            self.tyresLabel.setText(newTyres.text)

    def _retireDone(self, reply):
        if reply != "ok":
            QMessageBox.warning(self, "Error", "Car retirement failed:\n\n   %s" % reply)
            self.retireButton.setEnabled(True)

    def _setConsumption(self, carId, _interm, _lap, fuel, tyresCons, tyresType):
        if carId == self._id:
            self._fuel = fuel
            self.fuelBar.setValue(round(self._fuel))
            self.tyresBar.setValue(round(100 - tyresCons))
            self.tyresLabel.setText(tyresType.text)

    def _setMaxFuel(self, maxFuel):
        self.fuelBar.setMaximum(maxFuel)
        if self._fuel is not None:
            self.fuelBar.setValue(round(self._fuel))

    def _setRetired(self, carId):
        if carId == self._id:
            self.setTitle(self.title() + "  [retired]")
            self.setEnabled(False)
