from PyQt4.Qt import Qt
from PyQt4.QtCore import QRectF
from PyQt4.QtGui import QGraphicsItem, QGraphicsColorizeEffect, QFont, QPen


__all__ = ['Car']

__carSize = 32


class Car(QGraphicsItem):

    def __init__(self, track, id, startPos=0, pitLane=False):
        QGraphicsItem.__init__(self)
        self.setAcceptHoverEvents(True)
        self.setCacheMode(QGraphicsItem.DeviceCoordinateCache)
        self.setOpacity(0.7)
        self.setZValue(4)
        self._track = track
        self._id = id
        self._effect = QGraphicsColorizeEffect()
        self._effect.setColor(Qt.darkMagenta)
        self._effect.setEnabled(False)
        self._font = QFont()
        self._font.setPointSize(16)
        self._pen = QPen(Qt.black, __carSize, Qt.SolidLine, Qt.RoundCap)
        self._rect = QRectF(-__carSize / 2, -__carSize / 2, __carSize, __carSize)
        self.setGraphicsEffect(self._effect)
        self.setToolTip("Car " + str(self._id))
        self.updatePos(startPos, pitLane)
        self._translateToNewPos()

    def advance(self, phase):
        if phase == 1:
            self._translateToNewPos()

    def boundingRect(self):
        return self._rect

    def hoverEnterEvent(self, event):
        self.graphicsEffect().setEnabled(True)
        self.setOpacity(1.0)
        QGraphicsItem.hoverEnterEvent(self, event)

    def hoverLeaveEvent(self, event):
        self.graphicsEffect().setEnabled(False)
        self.setOpacity(0.7)
        QGraphicsItem.hoverLeaveEvent(self, event)

    def paint(self, painter, _option, _widget):
        painter.setPen(self._pen)
        painter.drawPoint(0, 0)
        painter.setPen(Qt.white)
        painter.setFont(self._font)
        painter.drawText(self._rect, Qt.AlignCenter | Qt.TextDontClip, str(self._id))

    def updatePos(self, pos, pit):
        self._position = pos
        self._pit = pit

    def _translateToNewPos(self):
        pos = self.mapFromScene(self._track.calculateCarPos(self._position, self._pit))
        self.translate(pos.x(), pos.y())
