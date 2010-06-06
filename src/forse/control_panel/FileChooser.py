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

from PyQt4.QtCore import QFileInfo, pyqtSlot
from PyQt4.QtGui import QCompleter, QDirModel, QFileDialog, QWidget
from Ui_FileChooser import Ui_FileChooser


class FileChooser(QWidget, Ui_FileChooser):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.setupUi(self)
        self.__completer = QCompleter(self)
        self.__completer.setModel(QDirModel(self.__completer))
        self.lineEdit.setCompleter(self.__completer)

    def fileName(self):
        return str(self.lineEdit.text())

    def setDefaultPath(self, path):
        self.lineEdit.setText(QFileInfo(path).canonicalFilePath())

    def setLabel(self, label):
        self.label.setText(label)

    @pyqtSlot(name="on_button_clicked")
    def _openFileDialog(self):
        filename = QFileDialog.getOpenFileName(self, "Choose configuration file")
        if filename:
            self.lineEdit.setText(QFileInfo(filename).canonicalFilePath())
