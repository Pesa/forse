import math
from PyQt4.Qt import Qt
from PyQt4.QtCore import QPointF, QRectF
from PyQt4.QtGui import QGraphicsItem, QGraphicsItemGroup
from PyQt4.QtGui import QPainterPath, QPainterPathStroker, QPen


trackWidth = 16
pitWidth = 12
pitDistance = -3 * trackWidth
pitPen = QPen(Qt.darkGray, pitWidth, Qt.SolidLine, Qt.FlatCap)


def strokeWithPen(path, pen):
    stroker = QPainterPathStroker()
    stroker.setCapStyle(pen.capStyle())
    stroker.setDashPattern(pen.style())
    stroker.setJoinStyle(pen.joinStyle())
    stroker.setWidth(pen.width())
    return stroker.createStroke(path)


class Sector(QGraphicsItem):

    def __init__(self, parent, pos, angle, color):
        QGraphicsItem.__init__(self, parent)
        self.setCacheMode(QGraphicsItem.DeviceCoordinateCache)
        self.initPos = pos
        self.length = 0
        self.pitLane = False
        self.pen = QPen(color, trackWidth, Qt.SolidLine, Qt.FlatCap)
        self.translate(pos.x(), pos.y())
        self.rotate(angle)

    def __len__(self):
        return self.length

    def boundingRect(self):
        try:
            return self.stroke.boundingRect()
        except AttributeError:
            return QRectF()

    def calculateCarPos(self, pos, _pit):
        try:
            percent = float(pos) / self.length
            return self.mapToScene(self.path.pointAtPercent(percent))
        except AttributeError:
            return self.mapToScene(0, 0)

    def enablePitLane(self):
        self.pitLane = True

    def finalState(self):
        return self.initPos, 0.0

    def paint(self, painter, _option, _widget):
        if self.pitLane:
            painter.setPen(pitPen)
            try:
                painter.drawPath(self.pitPath)
            except AttributeError:
                pass
        painter.setPen(self.pen)
        painter.drawPath(self.path)

    def shape(self):
        try:
            return self.stroke
        except AttributeError:
            return QPainterPath()


class BentSector(Sector):

    def __init__(self, parent, pos, angle, color, length, radius):
        Sector.__init__(self, parent, pos, angle, color)
        if length <= 0:
            raise ValueError("'length' must be a positive value.")
        if radius == 0:
            raise ValueError("'radius' must be non-zero.")
        self.setZValue(2)
        self.angle = 180.0 * length / (math.pi * radius)
        self.length = length
        self.radius = radius
        self.startAngle = 180.0 if radius > 0 else 0
        rect = QRectF(2 * radius if radius < 0 else 0, -abs(radius),
                      2 * abs(radius), 2 * abs(radius))
        self.path = QPainterPath()
        self.path.arcTo(rect, self.startAngle, self.angle)
        self.stroke = strokeWithPen(self.path, self.pen)

    def enablePitLane(self):
        Sector.enablePitLane(self)
        pitRadius = self.radius - pitDistance
        pitRect = QRectF(pitDistance + (2 * pitRadius if pitRadius < 0 else 0),
                         - abs(pitRadius), 2 * abs(pitRadius), 2 * abs(pitRadius))
        self.pitPath = QPainterPath(QPointF(pitDistance, 0))
        self.pitPath.arcTo(pitRect, self.startAngle, self.angle)
        pitStroke = strokeWithPen(self.pitPath, pitPen)
        self.stroke += pitStroke

    def finalState(self):
        return self.mapToParent(self.path.currentPosition()), -self.angle


class StraightSector(Sector):

    def __init__(self, parent, pos, angle, color, length):
        Sector.__init__(self, parent, pos, angle, color)
        if length <= 0:
            raise ValueError("'length' must be a positive value.")
        self.setZValue(2)
        self.length = length
        self.path = QPainterPath()
        self.path.lineTo(0, self.length)
        self.stroke = strokeWithPen(self.path, self.pen)

    def enablePitLane(self):
        Sector.enablePitLane(self)
        self.pitPath = QPainterPath(QPointF(pitDistance, 0))
        self.pitPath.lineTo(pitDistance, self.length)
        pitStroke = strokeWithPen(self.pitPath, pitPen)
        self.stroke += pitStroke

    def finalState(self):
        return self.mapToParent(self.path.currentPosition()), 0.0


class PitLaneAccess(Sector):

    def __init__(self, parent, pos, angle, color):
        Sector.__init__(self, parent, pos, angle, color)
        self.setZValue(1)
        self.path = QPainterPath()
        self.path.lineTo(pitDistance, 0)
        self.pen = QPen(pitPen)
        self.pen.setCapStyle(Qt.SquareCap)
        self.stroke = strokeWithPen(self.path, self.pen)


class PitLaneEntrance(PitLaneAccess):

    def __init__(self, parent, pos, angle, color):
        PitLaneAccess.__init__(self, parent, pos, angle, color)


class PitLaneExit(PitLaneAccess):

    def __init__(self, parent, pos, angle, color):
        PitLaneAccess.__init__(self, parent, pos, angle, color)


class FinishLine(Sector):

    def __init__(self, parent, pos, angle, color):
        Sector.__init__(self, parent, pos, angle, color)
        self.setZValue(3)
        self.blackPen = QPen(Qt.black, trackWidth / 2, Qt.SolidLine, Qt.FlatCap)
        self.whitePen = QPen(Qt.white, trackWidth / 2, Qt.SolidLine, Qt.FlatCap)
        self._createPaths(trackWidth * 3)

    def _createPaths(self, width):
        step = trackWidth / 2
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
        self.stroke = strokeWithPen(self.blackPath, self.blackPen)
        self.stroke += strokeWithPen(self.whitePath, self.whitePen)

    def enablePitLane(self):
        Sector.enablePitLane(self)
        self._createPaths(trackWidth * 3 + abs(pitDistance))

    def paint(self, painter, _option, _widget):
        painter.setPen(self.blackPen)
        painter.drawPath(self.blackPath)
        painter.setPen(self.whitePen)
        painter.drawPath(self.whitePath)


class Track(QGraphicsItemGroup):

    def __init__(self, sectors):
        QGraphicsItemGroup.__init__(self)
        self._nextColor = 5
        self._sectors = self._sectorsToItems(sectors, QPointF(), 90.0,
                                             self._getNextColor(), [])
        self._totalLength = 0
        for s in self._sectors:
            self.addToGroup(s)
            self._totalLength += len(s)

    def _getNextColor(self):
        c = self._nextColor + 7
        self._nextColor = (self._nextColor + 1) % 12
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
                item = FinishLine(self, pos, angle, color)
                color = self._getNextColor()
            elif type.text == "intermediate":
                color = self._getNextColor()
            elif type.text == "pitlane_entrance":
                item = PitLaneEntrance(self, pos, angle, color)
            elif type.text == "pitlane_exit":
                item = PitLaneExit(self, pos, angle, color)
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

    def calculateCarPos(self, pos, pit):
        normalized = pos % self._totalLength
        current = 0
        for s in self._sectors:
            relative = normalized - current
            if relative < len(s):
                return s.calculateCarPos(relative, pit)
            current += len(s)
