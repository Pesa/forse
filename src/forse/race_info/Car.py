from PyQt4.Qt import Qt
from PyQt4.QtCore import QRectF
from PyQt4.QtGui import QGraphicsItem, QGraphicsColorizeEffect, QFont, QPen
from PilotInfo import PilotInfo


class Car(QGraphicsItem):

    carSize = 32

    def __init__(self, track, carId, startPos=0, pitLane=False):
        QGraphicsItem.__init__(self)
        self.setAcceptHoverEvents(True)
        self.setCacheMode(QGraphicsItem.DeviceCoordinateCache)
        self.setOpacity(0.7)
        self.setZValue(4)
        self._track = track
        self._id = carId
        self._position = startPos
        self._pit = pitLane
        self._font = QFont()
        self._font.setPointSize(16)
        self._pen = QPen(Qt.black, self.carSize, Qt.SolidLine, Qt.RoundCap)
        self._rect = QRectF(-self.carSize / 2, -self.carSize / 2,
                            self.carSize, self.carSize)
        self._translateToNewPos()
        self.refreshToolTip()
        e = QGraphicsColorizeEffect()
        e.setColor(Qt.darkMagenta)
        e.setEnabled(False)
        self.setGraphicsEffect(e)

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

    def refreshToolTip(self):
        tooltip = "Car %i (%s)\n%s - %s" % (self._id,
                                            PilotInfo.get(self._id).state(),
                                            PilotInfo.get(self._id).name(),
                                            PilotInfo.get(self._id).teamName())
        self.setToolTip(tooltip)

    def updatePos(self, pos, pit):
        self._position = pos
        self._pit = pit

    def _translateToNewPos(self):
        self.setPos(self._track.projection(self._position, self._pit))
