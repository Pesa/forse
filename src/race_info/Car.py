from PyQt4.Qt import Qt
from PyQt4.QtGui import QPen


class Car(object):

    def __init__(self, id, startPos=0):
        object.__init__(self)
        self._id = id
        self.position = startPos

    def draw(self, painter):
        painter.setPen(QPen(Qt.yellow, 20, Qt.SolidLine, Qt.RoundCap))
        painter.drawPoint(0, 0)
