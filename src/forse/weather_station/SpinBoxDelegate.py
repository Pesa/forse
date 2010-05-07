from PyQt4.Qt import Qt
from PyQt4.QtGui import QSpinBox, QStyledItemDelegate


class SpinBoxDelegate(QStyledItemDelegate):

    def __init__(self, parent=None):
        QStyledItemDelegate.__init__(self, parent)

    def createEditor(self, parent, _option, _index):
        editor = QSpinBox(parent)
        editor.setRange(0, 10)
        return editor

    def setEditorData(self, editor, index):
        val, ok = index.data(Qt.EditRole).toInt()
        if ok:
            editor.setValue(val)

    def setModelData(self, editor, model, index):
        model.setData(index, editor.value(), Qt.EditRole)

    def updateEditorGeometry(self, editor, option, _index):
        editor.setGeometry(option.rect)
