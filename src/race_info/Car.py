from PyQt4.Qt import Qt
from PyQt4.QtCore import QRectF
from PyQt4.QtGui import QGraphicsItem, QFont, QPen


class Car(QGraphicsItem):

    def __init__(self, track, id, startPos=0):
        QGraphicsItem.__init__(self)
        self.setZValue(4)
        self._track = track
        self._id = id
        self.position = startPos

    def advance(self, phase):
        if phase == 0:
            return
        # TODO: implement me!
        #self.translate(self.position.x(), self.position.y())

    def boundingRect(self):
        return QRectF(-15, -15, 30, 30)

    def paint(self, painter, _option, _widget):
        painter.setPen(QPen(Qt.yellow, 30, Qt.SolidLine, Qt.RoundCap))
        painter.drawPoint(0, 0)
        painter.setPen(Qt.black)
        painter.setFont(QFont("Helvetica", 18))
        painter.drawText(0, 0, str(self._id))
