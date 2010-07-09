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
        self._retired = False # TODO
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
        tooltip = "Car %i" % self._id
        if PilotInfo.get(self._id).state() == "retired":
            tooltip += " (retired)"
        tooltip += "\n%s - %s" % (PilotInfo.get(self._id).name(),
                                  PilotInfo.get(self._id).teamName())
        self.setToolTip(tooltip)

    def updatePos(self, pos, pit):
        self._position = pos
        self._pit = pit

    def _translateToNewPos(self):
        self.setPos(self._track.projection(self._position, self._pit, self._retired))
