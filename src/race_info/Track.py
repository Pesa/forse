import math
from PyQt4.Qt import Qt
from PyQt4.QtCore import QPointF, QRectF
from PyQt4.QtGui import QGraphicsItem, QGraphicsItemGroup
from PyQt4.QtGui import QPainterPath, QPainterPathStroker, QPen


trackWidth = 16
pitWidth = 8
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
        #self.setCacheMode(QGraphicsItem.DeviceCoordinateCache)
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

    def enablePitLane(self):
        self.pitLane = True

    def finalState(self):
        return self.initPos, 0.0

    def paint(self, painter, option, widget):
        pass

    def shape(self):
        try:
            return self.stroke
        except AttributeError:
            return QPainterPath()

#    def translate(self, painter, position):
#        pass


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

    def paint(self, painter, _option, _widget):
        if self.pitLane:
            painter.setPen(pitPen)
            painter.drawPath(self.pitPath)
        painter.setPen(self.pen)
        painter.drawPath(self.path)

#    def translate(self, painter, position):
#        relativeAngle = math.radians(180.0 * position / (math.pi * self.radius))
#        painter.translate(self.sign * self.radius * (1 - math.cos(relativeAngle)),
#                          self.radius * math.sin(relativeAngle))


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

    def paint(self, painter, _option, _widget):
        if self.pitLane:
            painter.setPen(pitPen)
            painter.drawPath(self.pitPath)
        painter.setPen(self.pen)
        painter.drawPath(self.path)

#    def translate(self, painter, position):
#        painter.translate(0, position)


class PitLaneAccess(Sector):

    def __init__(self, parent, pos, angle, color):
        Sector.__init__(self, parent, pos, angle, color)
        self.setZValue(1)
        self.path = QPainterPath()
        self.path.lineTo(pitDistance, 0)
        self.pen = QPen(pitPen)
        self.pen.setCapStyle(Qt.SquareCap)
        self.stroke = strokeWithPen(self.path, self.pen)

    def paint(self, painter, _option, _widget):
        painter.setPen(self.pen)
        painter.drawPath(self.path)


class PitLaneEntrance(PitLaneAccess):

    def __init__(self, parent, pos, angle, color):
        PitLaneAccess.__init__(self, parent, pos, angle, color)


class PitLaneExit(PitLaneAccess):

    def __init__(self, parent, pos, angle, color):
        PitLaneAccess.__init__(self, parent, pos, angle, color)


class Intermediate(Sector):

    def __init__(self, parent, pos, angle, color):
        Sector.__init__(self, parent, pos, angle, color)


class FinishLine(Intermediate):

    def __init__(self, parent, pos, angle, color):
        Intermediate.__init__(self, parent, pos, angle, color)
        self.setZValue(3)
        self.path = QPainterPath(QPointF(-20, 0))
        self.path.lineTo(20, 0)
        self.pen = QPen(Qt.white, 20)
        self.stroke = strokeWithPen(self.path, self.pen)

    def paint(self, painter, _option, _widget):
        painter.setPen(self.pen)
        painter.drawPath(self.path)


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
            self._processPitLane(items, False)
            return items
        sector = sectors.pop(0)
        item = None
        if len(sector) == 1:
            type, = sector
            if type.text == "finish_line":
                item = FinishLine(self, pos, angle, color)
                color = self._getNextColor()
            elif type.text == "intermediate":
                item = Intermediate(self, pos, angle, color)
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
        if item is None:
            raise ValueError("invalid sector " + str(sector))
        newpos, newangle = item.finalState()
        items.append(item)
        return self._sectorsToItems(sectors, newpos, (angle + newangle) % 360,
                                    color, items)

#    def draw(self, cars):
#        if not cars:
#            return self._renderTrack()
#        pic = QPicture()
#        painter = QPainter()
#        painter.begin(pic)
#        self._renderTrack().play(painter)
#        currentPos = 0
#        for s in self._sectors:
#            for car in cars[:]:
#                relativePos = car.position % self._totalLength - currentPos
#                if relativePos < len(s):
#                    painter.save()
#                    s.translate(painter, relativePos)
#                    car.draw(painter)
#                    painter.restore()
#                    cars.remove(car)
#            s.finalize(painter)
#            currentPos += len(s)
#        painter.end()
#        return pic
