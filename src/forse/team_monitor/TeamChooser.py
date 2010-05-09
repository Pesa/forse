from PyQt4.QtGui import QDialog, QDialogButtonBox
from OTPApplication import OTPApplication
from Util import listToString
from Ui_TeamChooser import Ui_TeamChooser


class TeamChooser(QDialog, Ui_TeamChooser):

    def __init__(self, parent=None):
        QDialog.__init__(self, parent)
        self.setupUi(self)
        self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(False)
        handlers = {('init', 'names'): self._teamsInit,
                    ('update', 'names'): self._teamsUpdate}
        OTPApplication.registerMsgHandlers(handlers)

    def chosenTeam(self):
        index = self.comboBox.currentIndex()
        value, _ = self.comboBox.itemData(index).toInt()
        return value, self.comboBox.currentText()

    def _teamsInit(self, teams):
        self.comboBox.clear()
        for teamId, name in teams:
            self.comboBox.addItem(listToString(name), teamId)
        self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(len(teams) > 0)

    def _teamsUpdate(self, team):
        teamId, name = team
        self.comboBox.addItem(listToString(name), teamId)
        self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(True)
