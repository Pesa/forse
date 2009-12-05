from PyQt4.Qt import Qt
from PyQt4.QtCore import QRectF
from PyQt4.QtGui import QGraphicsItem, QFont, QPen

carSize = 32

class Car(QGraphicsItem):

    def __init__(self, track, id, startPos=0):
        QGraphicsItem.__init__(self)
        self.setCacheMode(QGraphicsItem.DeviceCoordinateCache)
        self.setOpacity(0.7)
        self.setZValue(4)
        self._id = id
        self._track = track
        self._font = QFont()
        self._font.setPointSize(16)
        self._pen = QPen(Qt.black, carSize, Qt.SolidLine, Qt.RoundCap)
        self._rect = QRectF(-carSize / 2, -carSize / 2, carSize, carSize)
        self._pit = False
        self.position = startPos
        self._updatePos()

    def _updatePos(self):
        pos = self.mapFromScene(self._track.calculateCarPos(self.position, self._pit))
        self.translate(pos.x(), pos.y())

    def advance(self, phase):
        if phase == 1:
            self._updatePos()

    def boundingRect(self):
        return self._rect

    def paint(self, painter, _option, _widget):
        painter.setPen(self._pen)
        painter.drawPoint(0, 0)
        painter.setPen(Qt.white)
        painter.setFont(self._font)
        painter.drawText(self._rect, Qt.AlignCenter | Qt.TextDontClip, str(self._id))
