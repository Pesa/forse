from PyQt4.QtCore import QFileInfo, QString, pyqtSlot
from PyQt4.QtGui import QCompleter, QDirModel, QFileDialog, QWidget
from Ui_FileChooser import Ui_FileChooser


class FileChooser(QWidget, Ui_FileChooser):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.setupUi(self)
        self.__fileName = QString("")
        self.__completer = QCompleter(self)
        self.__completer.setModel(QDirModel(self.__completer))
        self.lineEdit.setCompleter(self.__completer)

    def getFileName(self):
        return str(self.__fileName)

    def setDefaultPath(self, path):
        canonicalized = QFileInfo(path).canonicalFilePath()
        self.__fileName = canonicalized
        self.lineEdit.setText(canonicalized)

    def setLabel(self, label):
        self.label.setText(label)

    @pyqtSlot(name="on_button_clicked")
    def __openFileDialog(self):
        filename = QFileDialog.getOpenFileName(self, "Choose configuration file")
        if filename:
            canonicalized = QFileInfo(filename).canonicalFilePath()
            self.__fileName = canonicalized
            self.lineEdit.setText(canonicalized)
