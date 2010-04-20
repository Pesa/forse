import math
from PyQt4.Qt import Qt
from PyQt4.QtCore import QPointF, QRectF
from PyQt4.QtGui import QGraphicsItem, QGraphicsItemGroup
from PyQt4.QtGui import QPainterPath, QPainterPathStroker, QPen


__all__ = ['Track']

_trackWidth = 16
_pitWidth = 12
_pitDistance = -3 * _trackWidth
_pitPen = QPen(Qt.darkGray, _pitWidth, Qt.SolidLine, Qt.FlatCap)


def _strokeWithPen(path, pen):
    stroker = QPainterPathStroker()
    stroker.setCapStyle(pen.capStyle())
    stroker.setDashPattern(pen.style())
    stroker.setJoinStyle(pen.joinStyle())
    stroker.setWidth(pen.width())
    return stroker.createStroke(path)


class Sector(QGraphicsItem):

    def __init__(self, parent, pos, angle):
        QGraphicsItem.__init__(self, parent)
        self.setCacheMode(QGraphicsItem.DeviceCoordinateCache)
        self.initPos = pos
        self.length = 0
        self.pitLane = False
        self.translate(pos.x(), pos.y())
        self.rotate(angle)

    def __len__(self):
        return self.length

    def boundingRect(self):
        return self.stroke.boundingRect()

    def enablePitLane(self):
        self.pitLane = True

    def finalState(self):
        return self.initPos, 0.0

    def shape(self):
        return self.stroke


class PhysicalSector(Sector):

    def __init__(self, parent, pos, angle, color, length):
        Sector.__init__(self, parent, pos, angle)
        if length <= 0:
            raise ValueError("'length' must be a positive value.")
        self.length = length
        self.pen = QPen(color, _trackWidth, Qt.SolidLine, Qt.FlatCap)
        self.setZValue(2)

    def paint(self, painter, _option, _widget):
        if self.pitLane:
            painter.setPen(_pitPen)
            painter.drawPath(self.pitPath)
        painter.setPen(self.pen)
        painter.drawPath(self.path)

    def projection(self, pos, pit):
        percent = float(pos) / self.length
        if pit and self.pitLane:
            point = self.pitPath.pointAtPercent(percent)
        else:
            point = self.path.pointAtPercent(percent)
        return self.mapToScene(point)


class BentSector(PhysicalSector):

    def __init__(self, parent, pos, angle, color, length, radius):
        PhysicalSector.__init__(self, parent, pos, angle, color, length)
        if radius == 0:
            raise ValueError("'radius' must be non-zero.")
        self.radius = radius
        self.angle = 180.0 * self.length / (math.pi * radius)
        self.startAngle = 180.0 if radius > 0 else 0
        rect = QRectF(2 * radius if radius < 0 else 0, -abs(radius),
                      2 * abs(radius), 2 * abs(radius))
        self.path = QPainterPath()
        self.path.arcTo(rect, self.startAngle, self.angle)
        self.stroke = _strokeWithPen(self.path, self.pen)

    def enablePitLane(self):
        PhysicalSector.enablePitLane(self)
        pitRadius = self.radius - _pitDistance
        pitRect = QRectF(_pitDistance + (2 * pitRadius if pitRadius < 0 else 0),
                         - abs(pitRadius), 2 * abs(pitRadius), 2 * abs(pitRadius))
        self.pitPath = QPainterPath(QPointF(_pitDistance, 0))
        self.pitPath.arcTo(pitRect, self.startAngle, self.angle)
        pitStroke = _strokeWithPen(self.pitPath, _pitPen)
        self.stroke += pitStroke

    def finalState(self):
        return self.mapToParent(self.path.currentPosition()), -self.angle


class StraightSector(PhysicalSector):

    def __init__(self, parent, pos, angle, color, length):
        PhysicalSector.__init__(self, parent, pos, angle, color, length)
        self.path = QPainterPath()
        self.path.lineTo(0, self.length)
        self.stroke = _strokeWithPen(self.path, self.pen)

    def enablePitLane(self):
        PhysicalSector.enablePitLane(self)
        self.pitPath = QPainterPath(QPointF(_pitDistance, 0))
        self.pitPath.lineTo(_pitDistance, self.length)
        pitStroke = _strokeWithPen(self.pitPath, _pitPen)
        self.stroke += pitStroke

    def finalState(self):
        return self.mapToParent(self.path.currentPosition()), 0.0


class PitLaneAccess(Sector):

    def __init__(self, parent, pos, angle):
        Sector.__init__(self, parent, pos, angle)
        self.setZValue(1)
        self.path = QPainterPath()
        self.path.lineTo(_pitDistance, 0)
        self.pen = QPen(_pitPen)
        self.pen.setCapStyle(Qt.SquareCap)
        self.stroke = _strokeWithPen(self.path, self.pen)

    def paint(self, painter, _option, _widget):
        painter.setPen(self.pen)
        painter.drawPath(self.path)


class PitLaneEntrance(PitLaneAccess):

    def __init__(self, parent, pos, angle):
        PitLaneAccess.__init__(self, parent, pos, angle)


class PitLaneExit(PitLaneAccess):

    def __init__(self, parent, pos, angle):
        PitLaneAccess.__init__(self, parent, pos, angle)


class FinishLine(Sector):

    def __init__(self, parent, pos, angle):
        Sector.__init__(self, parent, pos, angle)
        self.setZValue(3)
        self.blackPen = QPen(Qt.black, _trackWidth / 2, Qt.SolidLine, Qt.FlatCap)
        self.whitePen = QPen(Qt.white, _trackWidth / 2, Qt.SolidLine, Qt.FlatCap)
        self._createPaths(_trackWidth * 3)

    def _createPaths(self, width):
        step = _trackWidth / 2
        halfstep = step / 2
        start = step * 3
        n = width / step
        self.blackPath = QPainterPath()
        for i in range(0, n):
            sign = 1 if i % 2 == 1 else - 1
            self.blackPath.moveTo(start - step * i, sign * halfstep)
            self.blackPath.lineTo(start - step * (i + 1), sign * halfstep)
        self.whitePath = QPainterPath()
        for i in range(0, n):
            sign = 1 if i % 2 == 0 else - 1
            self.whitePath.moveTo(start - step * i, sign * halfstep)
            self.whitePath.lineTo(start - step * (i + 1), sign * halfstep)
        self.stroke = _strokeWithPen(self.blackPath, self.blackPen)
        self.stroke += _strokeWithPen(self.whitePath, self.whitePen)

    def enablePitLane(self):
        Sector.enablePitLane(self)
        self._createPaths(_trackWidth * 3 + abs(_pitDistance))

    def paint(self, painter, _option, _widget):
        painter.setPen(self.blackPen)
        painter.drawPath(self.blackPath)
        painter.setPen(self.whitePen)
        painter.drawPath(self.whitePath)


class Track(QGraphicsItemGroup):

    def __init__(self, sectors):
        QGraphicsItemGroup.__init__(self)
        self.__nextColor = 5
        self.__sectors = self._sectorsToItems(sectors, QPointF(), 90.0, self._nextColor(), [])
        self.__totalLength = 0
        for s in self.__sectors:
            self.addToGroup(s)
            self.__totalLength += len(s)

    def projection(self, pos, pit):
        """
        Returns the position of an object on the track in scene coordinates.
        """
        normalized = pos % self.__totalLength
        current = 0
        for s in self.__sectors:
            relative = normalized - current
            if relative < len(s):
                return s.projection(relative, pit)
            current += len(s)

    def _nextColor(self):
        c = self.__nextColor + 7
        self.__nextColor = (self.__nextColor + 1) % 12
        return Qt.GlobalColor(c)

    def _processPitLane(self, sectors, pit):
        if not sectors:
            return
        sector = sectors.pop(0)
        if isinstance(sector, PitLaneEntrance):
            self._processPitLane(sectors, True)
        elif not pit:
            sectors.append(sector)
            self._processPitLane(sectors, False)
        elif isinstance(sector, PitLaneExit):
            return
        else:
            sector.enablePitLane()
            self._processPitLane(sectors, True)

    def _sectorsToItems(self, sectors, pos, angle, color, items):
        if not sectors:
            self._processPitLane(items[:], False)
            return items
        sector = sectors.pop(0)
        item = None
        if len(sector) == 1:
            type, = sector
            if type.text == "finish_line":
                item = FinishLine(self, pos, angle)
                color = self._nextColor()
            elif type.text == "intermediate":
                color = self._nextColor()
            elif type.text == "pitlane_entrance":
                item = PitLaneEntrance(self, pos, angle)
            elif type.text == "pitlane_exit":
                item = PitLaneExit(self, pos, angle)
        elif len(sector) == 6:
            type, length, _minLane, _maxLane, _incl, _rain = sector
            if type.text == "straight":
                item = StraightSector(self, pos, angle, color, length)
        elif len(sector) == 7:
            type, length, curv, _minLane, _maxLane, _incl, _rain = sector
            if type.text == "left":
                item = BentSector(self, pos, angle, color, length, curv)
            elif type.text == "right":
                item = BentSector(self, pos, angle, color, length, -curv)
        if item is not None:
            pos, newangle = item.finalState()
            angle = (angle + newangle) % 360
            items.append(item)
        return self._sectorsToItems(sectors, pos, angle, color, items)
