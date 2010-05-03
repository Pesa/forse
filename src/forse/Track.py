import math
from PyQt4.Qt import Qt
from PyQt4.QtCore import QPointF, QRectF
from PyQt4.QtGui import QGraphicsDropShadowEffect, QGraphicsItemGroup, QGraphicsPathItem, QPainterPath, QPen
from Util import atomToBool


__all__ = ['Track']

_trackWidth = 16
_pitWidth = 12
_pitDistance = -3 * _trackWidth
_pitColor = Qt.darkGray
_pitPen = QPen(_pitColor, _pitWidth, Qt.SolidLine, Qt.FlatCap)
_hoverColor = Qt.green


class AbstractSector(object):

    def __init__(self, pos, angle, length=0):
        object.__init__(self)
        self._length = length
        self.setCacheMode(QGraphicsPathItem.DeviceCoordinateCache)
        self.setPos(pos)
        self.setRotation(angle)

    def finalLocation(self):
        return self.pos(), 0.0

    def length(self):
        return self._length

    def setColor(self, color):
        pass


class Sector(QGraphicsPathItem, AbstractSector):

    def __init__(self, parent, pos, angle, length=0):
        QGraphicsPathItem.__init__(self, parent)
        AbstractSector.__init__(self, pos, angle, length)


class PhysicalSector(Sector):

    def __init__(self, parent, pos, angle, length, color, pit=False):
        Sector.__init__(self, parent, pos, angle, length)
        self.setZValue(2)
        if pit:
            self.setPen(_pitPen)
        else:
            self.setPen(QPen(color, _trackWidth, Qt.SolidLine, Qt.FlatCap))

    def enableHoverEffect(self):
        e = QGraphicsDropShadowEffect()
        e.setEnabled(False)
        e.setBlurRadius(2 * _trackWidth)
        e.setColor(_hoverColor)
        e.setOffset(0, 0)
        self.setGraphicsEffect(e)
        self.setAcceptHoverEvents(True)

    def hoverEnterEvent(self, event):
        self.graphicsEffect().setEnabled(True)
        Sector.hoverEnterEvent(self, event)

    def hoverLeaveEvent(self, event):
        self.graphicsEffect().setEnabled(False)
        Sector.hoverLeaveEvent(self, event)

    def projection(self, pos, _pit=False):
        percent = float(pos) / self.length()
        point = self.path().pointAtPercent(percent)
        return self.mapToScene(point)

    def setColor(self, color):
        pen = QPen(self.pen())
        pen.setColor(color)
        self.setPen(pen)


class BentSector(PhysicalSector):

    def __init__(self, parent, pos, angle, length, color, radius, pit=False):
        PhysicalSector.__init__(self, parent, pos, angle, length, color, pit)
        self.angle = 180.0 * self.length() / (math.pi * radius)
        startAngle = 180.0 if radius > 0 else 0
        offset = _pitDistance if pit else 0
        radius -= offset
        rect = QRectF(offset + (2 * radius if radius < 0 else 0), -abs(radius),
                      2 * abs(radius), 2 * abs(radius))
        path = QPainterPath(QPointF(offset, 0))
        path.arcTo(rect, startAngle, self.angle)
        self.setPath(path)

    def finalLocation(self):
        return self.mapToParent(self.path().currentPosition()), -self.angle


class StraightSector(PhysicalSector):

    def __init__(self, parent, pos, angle, length, color, pit=False):
        PhysicalSector.__init__(self, parent, pos, angle, length, color, pit)
        offset = _pitDistance if pit else 0
        path = QPainterPath(QPointF(offset, 0))
        path.lineTo(offset, self.length())
        self.setPath(path)

    def finalLocation(self):
        return self.mapToParent(self.path().currentPosition()), 0.0


class Intermediate(Sector):

    def __init__(self, parent, pos, angle):
        Sector.__init__(self, parent, pos, angle)


class FinishLine(QGraphicsItemGroup, AbstractSector):

    def __init__(self, parent, pos, angle, pit=False):
        QGraphicsItemGroup.__init__(self, parent)
        AbstractSector.__init__(self, pos, angle)
        self.setZValue(3)
        self.black = QGraphicsPathItem(self)
        self.white = QGraphicsPathItem(self)
        start = 3 * (_trackWidth / 2)
        end = -start - abs(_pitDistance if pit else 0)
        rowdelta = _trackWidth / 4
        for item, y in [(self.black, -rowdelta),
                        (self.white, rowdelta)]:
            item.setCacheMode(QGraphicsPathItem.DeviceCoordinateCache)
            self.addToGroup(item)
            path = QPainterPath()
            path.moveTo(start, y)
            path.lineTo(end, y)
            path.moveTo(end, -y)
            path.lineTo(start, -y)
            item.setPath(path)
        pen = QPen(Qt.black, _trackWidth / 2)
        pen.setCapStyle(Qt.FlatCap)
        pen.setDashPattern([1, 1])
        self.black.setPen(QPen(pen))
        pen.setColor(Qt.white)
        self.white.setPen(pen)


class PitLaneAccess(Sector):

    def __init__(self, parent, pos, angle):
        Sector.__init__(self, parent, pos, angle)
        self.setZValue(1)
        path = QPainterPath()
        path.lineTo(_pitDistance, 0)
        self.setPath(path)
        self.setPen(QPen(_pitColor, _pitWidth))


class PitLaneEntrance(PitLaneAccess):

    def __init__(self, parent, pos, angle):
        PitLaneAccess.__init__(self, parent, pos, angle)


class PitLaneExit(PitLaneAccess):

    def __init__(self, parent, pos, angle):
        PitLaneAccess.__init__(self, parent, pos, angle)


class SectorWithPitlane(QGraphicsItemGroup):

    def __init__(self, parent, sectype, *args):
        QGraphicsItemGroup.__init__(self, parent)
        self.setHandlesChildEvents(False)
        self.setZValue(2)
        self.pit = sectype(self, *args, pit=True)
        self.regular = sectype(self, *args, pit=False)
        self.addToGroup(self.pit)
        self.addToGroup(self.regular)
        # act as a proxy for almost all methods
        self.enableHoverEffect = self.regular.enableHoverEffect
        self.finalLocation = self.regular.finalLocation
        self.length = self.regular.length
        self.setColor = self.regular.setColor
        self.setToolTip = self.regular.setToolTip

    def projection(self, pos, pit):
        if pit:
            return self.pit.projection(pos)
        else:
            return self.regular.projection(pos)


class BentSectorWithPitlane(SectorWithPitlane):

    def __init__(self, parent, pos, angle, length, color, radius):
        SectorWithPitlane.__init__(self, parent, BentSector, pos, angle, length, color, radius)


class StraightSectorWithPitlane(SectorWithPitlane):

    def __init__(self, parent, pos, angle, length, color):
        SectorWithPitlane.__init__(self, parent, StraightSector, pos, angle, length, color)


class Track(QGraphicsItemGroup):

    def __init__(self, sectors, colorfunc):
        QGraphicsItemGroup.__init__(self)
        self.setHandlesChildEvents(False)
        sectors = self._sectorsToItems(sectors, colorfunc)
        self.__sectors = []
        self.__totalLength = 0
        for s in sectors:
            self.addToGroup(s)
            if isinstance(s, FinishLine):
                self.__offset = self.__totalLength
            elif s.length() > 0:
                self.__sectors.append(s)
                self.__totalLength += s.length()

    def enableHoverEffect(self):
        for s in self.__sectors:
            s.enableHoverEffect()

    def projection(self, pos, pit):
        """
        Returns the position of an object on the track in scene coordinates.
        """
        normalized = (pos + self.__offset) % self.__totalLength
        current = 0
        for s in self.__sectors:
            relative = normalized - current
            if relative < s.length():
                return s.projection(relative, pit)
            current += s.length()

    def setSectorColor(self, sectId, color):
        self.__sectors[sectId].setColor(color)

    def setSectorToolTip(self, sectId, tooltip):
        self.__sectors[sectId].setToolTip(tooltip)

    def _sectorsToItems(self, sectors, nextColor):
        color = nextColor()
        items = []
        pos, angle = QPointF(), 90.0
        for sector in sectors:
            item = None
            if len(sector) == 1:
                type, = sector
                if type.text == "intermediate":
                    item = Intermediate(self, pos, angle)
                    color = nextColor()
                elif type.text == "pitlane_entrance":
                    item = PitLaneEntrance(self, pos, angle)
                elif type.text == "pitlane_exit":
                    item = PitLaneExit(self, pos, angle)
            elif len(sector) == 2:
                type, pit = sector
                if type.text == "finish_line":
                    item = FinishLine(self, pos, angle, atomToBool(pit))
                    color = nextColor()
            elif len(sector) == 3:
                type, length, pit = sector
                if type.text == "straight":
                    ctor = StraightSectorWithPitlane if atomToBool(pit) else StraightSector
                    item = ctor(self, pos, angle, length, color)
            elif len(sector) == 4:
                type, length, curv, pit = sector
                ctor = BentSectorWithPitlane if atomToBool(pit) else BentSector
                if type.text == "left":
                    item = ctor(self, pos, angle, length, color, curv)
                elif type.text == "right":
                    item = ctor(self, pos, angle, length, color, -curv)
            if item is not None:
                pos, newangle = item.finalLocation()
                angle = (angle + newangle) % 360
                items.append(item)
        # fix coloring of the last intermediate
        for item in items:
            if isinstance(item, Intermediate) or isinstance(item, FinishLine):
                break
            item.setColor(color)
        return items
