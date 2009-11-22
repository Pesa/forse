import math
from PyQt4.Qt import Qt
from PyQt4.QtCore import QPointF, QRectF
from PyQt4.QtGui import QPainter, QPen, QPicture


class Sector(object):

    def __init__(self):
        object.__init__(self)

    def __len__(self):
        return 0

    def draw(self, painter):
        raise NotImplementedError()

    def finalize(self, painter):
        pass

    def translate(self, painter, position):
        pass


class BentSector(Sector):

    def __init__(self, length, radius):
        Sector.__init__(self)
        if length <= 0:
            raise ValueError("'length' must be a positive value.")
        self.length = length
        self.radius = abs(radius)
        self.angle = 180.0 * length / (math.pi * self.radius)
        self.sign = -1 if radius < 0 else 1
        self.boundingRect = QRectF(self.radius * (self.sign - 1),
                                   - self.radius,
                                   2 * self.radius,
                                   2 * self.radius)
        self.startAngle = 180.0 * 16 if self.sign == 1 else 0
        self.finalPoint = QPointF(self.sign * self.radius * (1 - math.cos(math.radians(self.angle))),
                                  self.radius * math.sin(math.radians(self.angle)))

    def __len__(self):
        return self.length

    def draw(self, painter):
        painter.drawArc(self.boundingRect, self.startAngle, self.sign * self.angle * 16)

    def finalize(self, painter):
        painter.translate(self.finalPoint)
        painter.rotate(-self.sign * self.angle)

    def translate(self, painter, position):
        relativeAngle = math.radians(180.0 * position / (math.pi * self.radius))
        painter.translate(self.sign * self.radius * (1 - math.cos(relativeAngle)),
                          self.radius * math.sin(relativeAngle))


class StraightSector(Sector):

    def __init__(self, length):
        Sector.__init__(self)
        if length <= 0:
            raise ValueError("'length' must be a positive value.")
        self.length = length

    def __len__(self):
        return self.length

    def draw(self, painter):
        painter.drawLine(0, 0, 0, self.length)

    def finalize(self, painter):
        painter.translate(0, self.length)

    def translate(self, painter, position):
        painter.translate(0, position)


class PitLaneEntrance(Sector):

    def __init__(self):
        Sector.__init__(self)

    def draw(self, painter):
        painter.save()
        painter.setPen(QPen(Qt.black, 5))
        painter.drawLine(-10, 0, 10, 0)
        painter.restore()


class PitLaneExit(Sector):

    def __init__(self):
        Sector.__init__(self)

    def draw(self, painter):
        painter.save()
        painter.setPen(QPen(Qt.black, 5))
        painter.drawLine(-10, 0, 10, 0)
        painter.restore()


class Intermediate(Sector):

    def __init__(self, color):
        Sector.__init__(self)
        self.pen = QPen(Qt.GlobalColor(color), 10, Qt.SolidLine, Qt.FlatCap, Qt.RoundJoin)

    def draw(self, painter):
        painter.setPen(self.pen)


class FinishLine(Sector):

    def __init__(self, color):
        Sector.__init__(self)
        self.pen = QPen(Qt.GlobalColor(color), 10, Qt.SolidLine, Qt.FlatCap, Qt.RoundJoin)

    def draw(self, painter):
        painter.save()
        painter.setPen(QPen(Qt.white, 5))
        painter.drawLine(-10, 0, 10, 0)
        painter.restore()
        painter.setPen(self.pen)


class Track(object):

    def __init__(self, sectors):
        object.__init__(self)
        self._nextColor = 0
        self._sectors = [ self._buildSector(s) for s in sectors ]

    def _buildSector(self, sector):
        if len(sector) == 1:
            type, = sector
            if type.text == "finish_line":
                return FinishLine(self._getNextColor())
            elif type.text == "intermediate":
                return Intermediate(self._getNextColor())
            elif type.text == "pitlane_entrance":
                return PitLaneEntrance()
            elif type.text == "pitlane_exit":
                return PitLaneExit()
        elif len(sector) == 6:
            type, length, _minLane, _maxLane, _incl, _rain = sector
            if type.text == "straight":
                return StraightSector(length)
        elif len(sector) == 7:
            type, length, curv, _minLane, _maxLane, _incl, _rain = sector
            if type.text == "left":
                return BentSector(length, curv)
            elif type.text == "right":
                return BentSector(length, -curv)
        raise ValueError("invalid sector " + str(sector))

    def _getNextColor(self):
        c = self._nextColor + 7
        self._nextColor = (self._nextColor + 1) % 12
        return c

    def draw(self):
        trackPicture = QPicture()
        painter = QPainter()
        painter.begin(trackPicture)
        painter.setRenderHint(QPainter.Antialiasing)
        painter.rotate(90)
        for s in self._sectors:
            s.draw(painter)
            s.finalize(painter)
        painter.end()
        return trackPicture
