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

    def getFileName(self):
        return str(self.lineEdit.text())

    def setDefaultPath(self, path):
        self.lineEdit.setText(QFileInfo(path).canonicalFilePath())

    def setLabel(self, label):
        self.label.setText(label)

    @pyqtSlot(name="on_button_clicked")
    def __openFileDialog(self):
        filename = QFileDialog.getOpenFileName(self, "Choose configuration file")
        if filename:
            self.lineEdit.setText(QFileInfo(filename).canonicalFilePath())
