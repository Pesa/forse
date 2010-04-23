import math
from PyQt4.Qt import Qt
from PyQt4.QtCore import QPointF, QRectF
from PyQt4.QtGui import QGraphicsItemGroup, QGraphicsPathItem, QPainterPath, QPen
from Util import atomToBool


__all__ = ['Track']

_trackWidth = 16
_pitWidth = 12
_pitDistance = -3 * _trackWidth
_pitPen = QPen(Qt.darkGray, _pitWidth, Qt.SolidLine, Qt.FlatCap)


class AbstractSector(object):

    def __init__(self, pos, angle, length=0):
        object.__init__(self)
        self._initialPos = pos
        self._length = length
        self.setCacheMode(QGraphicsPathItem.DeviceCoordinateCache)
        self.translate(pos.x(), pos.y())
        self.rotate(angle)

    def finalLocation(self):
        return self._initialPos, 0.0

    def length(self):
        return self._length


class Sector(QGraphicsPathItem, AbstractSector):

    def __init__(self, parent, pos, angle, length=0):
        QGraphicsPathItem.__init__(self, parent)
        AbstractSector.__init__(self, pos, angle, length)


class PhysicalSector(Sector):

    def __init__(self, parent, pos, angle, color, length, pit=False):
        Sector.__init__(self, parent, pos, angle, length)
        self.setZValue(2)
        if pit:
            self.setPen(_pitPen)
        else:
            self.setPen(QPen(color, _trackWidth, Qt.SolidLine, Qt.FlatCap))

    def projection(self, pos, _pit=False):
        percent = float(pos) / self.length()
        point = self.path().pointAtPercent(percent)
        return self.mapToScene(point)


class BentSector(PhysicalSector):

    def __init__(self, parent, pos, angle, color, length, radius, pit=False):
        PhysicalSector.__init__(self, parent, pos, angle, color, length, pit)
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

    def __init__(self, parent, pos, angle, color, length, pit=False):
        PhysicalSector.__init__(self, parent, pos, angle, color, length, pit)
        offset = _pitDistance if pit else 0
        path = QPainterPath(QPointF(offset, 0))
        path.lineTo(offset, self.length())
        self.setPath(path)

    def finalLocation(self):
        return self.mapToParent(self.path().currentPosition()), 0.0


class PitLaneAccess(Sector):

    def __init__(self, parent, pos, angle):
        Sector.__init__(self, parent, pos, angle)
        self.setZValue(1)
        path = QPainterPath()
        path.lineTo(_pitDistance, 0)
        self.setPath(path)
        pen = QPen(_pitPen)
        pen.setCapStyle(Qt.SquareCap)
        self.setPen(pen)


class PitLaneEntrance(PitLaneAccess):

    def __init__(self, parent, pos, angle):
        PitLaneAccess.__init__(self, parent, pos, angle)


class PitLaneExit(PitLaneAccess):

    def __init__(self, parent, pos, angle):
        PitLaneAccess.__init__(self, parent, pos, angle)


class FinishLine(QGraphicsItemGroup, AbstractSector):

    def __init__(self, parent, pos, angle, pit=False):
        QGraphicsItemGroup.__init__(self, parent)
        AbstractSector.__init__(self, pos, angle)
        self.setZValue(3)
        step = _trackWidth / 2
        halfstep = step / 2
        start = step * 3
        n = (_trackWidth * 3 + abs(_pitDistance if pit else 0)) / step
        self.black = QGraphicsPathItem(self)
        self.black.setCacheMode(QGraphicsPathItem.DeviceCoordinateCache)
        blackPath = QPainterPath()
        for i in range(0, n):
            sign = 1 if i % 2 == 1 else - 1
            blackPath.moveTo(start - step * i, sign * halfstep)
            blackPath.lineTo(start - step * (i + 1), sign * halfstep)
        self.black.setPath(blackPath)
        self.black.setPen(QPen(Qt.black, _trackWidth / 2, Qt.SolidLine, Qt.FlatCap))
        self.addToGroup(self.black)
        self.white = QGraphicsPathItem(self)
        self.white.setCacheMode(QGraphicsPathItem.DeviceCoordinateCache)
        whitePath = QPainterPath()
        for i in range(0, n):
            sign = 1 if i % 2 == 0 else - 1
            whitePath.moveTo(start - step * i, sign * halfstep)
            whitePath.lineTo(start - step * (i + 1), sign * halfstep)
        self.white.setPath(whitePath)
        self.white.setPen(QPen(Qt.white, _trackWidth / 2, Qt.SolidLine, Qt.FlatCap))
        self.addToGroup(self.white)


class SectorWithPitlane(QGraphicsItemGroup):

    def __init__(self, parent, sectype, *args):
        QGraphicsItemGroup.__init__(self, parent)
        self.setZValue(2)
        self.pit = sectype(self, *args, pit=True)
        self.regular = sectype(self, *args, pit=False)
        self.addToGroup(self.pit)
        self.addToGroup(self.regular)
        self.finalLocation = self.regular.finalLocation
        self.length = self.regular.length

    def projection(self, pos, pit):
        if pit:
            return self.pit.projection(pos)
        else:
            return self.regular.projection(pos)


class BentSectorWithPitlane(SectorWithPitlane):

    def __init__(self, parent, pos, angle, color, length, radius):
        SectorWithPitlane.__init__(self, parent, BentSector, pos, angle, color, length, radius)


class StraightSectorWithPitlane(SectorWithPitlane):

    def __init__(self, parent, pos, angle, color, length):
        SectorWithPitlane.__init__(self, parent, StraightSector, pos, angle, color, length)


class Track(QGraphicsItemGroup):

    def __init__(self, sectors):
        QGraphicsItemGroup.__init__(self)
        self.__sectors = self._sectorsToItems(sectors)
        while not isinstance(self.__sectors[0], FinishLine):
            self.__sectors.append(self.__sectors.pop(0))
        self.__totalLength = 0
        for s in self.__sectors:
            self.addToGroup(s)
            self.__totalLength += s.length()

    def projection(self, pos, pit):
        """
        Returns the position of an object on the track in scene coordinates.
        """
        normalized = pos % self.__totalLength
        current = 0
        for s in self.__sectors:
            relative = normalized - current
            if relative < s.length():
                return s.projection(relative, pit)
            current += s.length()

    def _color(self):
        color = 5
        while True:
            yield Qt.GlobalColor(color + 7)
            color = (color + 1) % 12

    def _sectorsToItems(self, sectors):
        items = []
        pos, angle = QPointF(), 90.0
        nextColor = self._color().next
        color = nextColor()
        for sector in sectors:
            item = None
            if len(sector) == 1:
                type, = sector
                if type.text == "intermediate":
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
                    item = ctor(self, pos, angle, color, length)
            elif len(sector) == 4:
                type, length, curv, pit = sector
                ctor = BentSectorWithPitlane if atomToBool(pit) else BentSector
                if type.text == "left":
                    item = ctor(self, pos, angle, color, length, curv)
                elif type.text == "right":
                    item = ctor(self, pos, angle, color, length, -curv)
            if item is not None:
                pos, newangle = item.finalLocation()
                angle = (angle + newangle) % 360
                items.append(item)
        return items
