import math
from PyQt4.Qt import Qt
from PyQt4.QtCore import QPointF, QRectF, pyqtSlot
from PyQt4.QtGui import QWidget, QPainter, QPen, QPicture


class Car(object):

	def __init__(self, carID):
		object.__init__(self)
		self.carID = carID
		self.position = 0

	def advance(self):
		self.position += 2 * self.carID

	def draw(self, painter):
		painter.setPen(QPen(Qt.white, 20, Qt.SolidLine, Qt.RoundCap))
		painter.drawPoint(0, 0)


class Sector(object):

	def __init__(self):
		object.__init__(self)

	def __len__(self):
		return 0

	def draw(self, painter, debug=False):
		raise NotImplementedError()

	def finalize(self, painter):
		pass

	def translate(self, painter, position):
		pass


class BentSector(Sector):

	def __init__(self, length, radius):
		Sector.__init__(self)
		if length <= 0: raise ValueError("'length' must be a positive value.")
		self.length = length
		self.radius = abs(radius)
		self.angle = 180.0 * length / (math.pi * self.radius)
		self.sign = -1 if radius < 0 else 1
		self.boundingRect = QRectF(self.radius * (self.sign - 1), -self.radius,
								   2 * self.radius, 2 * self.radius)
		self.startAngle = 180.0 * 16 if self.sign == 1 else 0
		self.finalPoint = QPointF(self.sign * self.radius * (1 - math.cos(math.radians(self.angle))),
								  self.radius * math.sin(math.radians(self.angle)))

	def __len__(self):
		return self.length

	def draw(self, painter, debug=False):
		if debug:
			painter.save()
			painter.setPen(QPen(Qt.red, 3, Qt.DotLine))
			painter.drawRect(self.boundingRect)
			painter.setPen(QPen(Qt.black, 10, Qt.SolidLine))
			painter.drawPoint(self.boundingRect.topLeft())
			painter.setPen(QPen(Qt.yellow, 15, Qt.SolidLine))
			painter.drawPoint(self.finalPoint)
			painter.restore()
		painter.drawArc(self.boundingRect, self.startAngle, self.sign * self.angle * 16)

	def finalize(self, painter):
		painter.translate(self.finalPoint)
		painter.rotate(-self.sign * self.angle)

	def translate(self, painter, position):
		relativeAngle = 180.0 * position / (math.pi * self.radius)
		painter.translate(self.sign * self.radius * (1 - math.cos(math.radians(relativeAngle))),
						  self.radius * math.sin(math.radians(relativeAngle)))


class StraightSector(Sector):

	def __init__(self, length):
		Sector.__init__(self)
		if length <= 0: raise ValueError("'length' must be a positive value.")
		self.length = length

	def __len__(self):
		return self.length

	def draw(self, painter, debug=False):
		painter.drawLine(0, 0, 0, self.length)

	def finalize(self, painter):
		painter.translate(0, self.length)

	def translate(self, painter, position):
		painter.translate(0, position)


class PitLaneEntrance(Sector):

	def __init__(self):
		Sector.__init__(self)

	def draw(self, painter, debug=False):
		painter.save()
		painter.setPen(QPen(Qt.yellow, 5))
		painter.drawLine(-10, 0, 10, 0)
		painter.restore()


class PitLaneExit(Sector):

	def __init__(self):
		Sector.__init__(self)

	def draw(self, painter, debug=False):
		painter.save()
		painter.setPen(QPen(Qt.yellow, 5))
		painter.drawLine(-10, 0, 10, 0)
		painter.restore()


class Intermediate(Sector):

	def __init__(self, color):
		Sector.__init__(self)
		self.color = color
		self.width = 10

	def draw(self, painter, debug=False):
		painter.setPen(QPen(self.color, self.width, Qt.SolidLine, Qt.FlatCap, Qt.RoundJoin))


class FinishLine(Sector):

	def __init__(self, color):
		Sector.__init__(self)
		self.color = color
		self.width = 10

	def draw(self, painter, debug=False):
		painter.save()
		painter.setPen(QPen(Qt.black, 5))
		painter.drawLine(-10, 0, 10, 0)
		painter.restore()
		painter.setPen(QPen(self.color, self.width, Qt.SolidLine, Qt.FlatCap, Qt.RoundJoin))


class TrackWidget(QWidget):

	def __init__(self, parent=None):
		QWidget.__init__(self, parent)
		self.debug = False
		self.cars = [] # [ Car(i) for i in xrange(2, 10) ]
		self.track = []
		self.totalLength = 0
		for segment in self.track:
			self.totalLength += len(segment)
		#self.clock = QTimer()
		#self.clock.timeout.connect(self.moveCars)
		#self.clock.start(100)

	def _buildSector(self, sector):
		if len(sector) == 1:
			type, = sector
			if type.text == "finish_line":
				return PitLaneEntrance() # FIXME: FinishLine()
			elif type.text == "intermediate":
				return PitLaneEntrance() # FIXME: Intermediate()
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

	def drawTrack(self):
		trackPicture = QPicture()
		painter = QPainter()
		painter.begin(trackPicture)
		painter.setRenderHint(QPainter.Antialiasing)
		painter.rotate(90)
		globalPos = 0
		remaining = self.cars[:]
		for segment in self.track:
			segment.draw(painter, self.debug)
			todo = remaining
			remaining = []
			for car in todo:
				if car.position % self.totalLength < globalPos + len(segment):
					painter.save()
					segment.translate(painter, car.position % self.totalLength - globalPos)
					car.draw(painter)
					painter.restore()
				else:
					remaining.append(car)
			segment.finalize(painter)
			globalPos += len(segment)
		painter.end()
		return trackPicture

	@pyqtSlot()
	def moveCars(self):
		for car in self.cars:
			car.advance()
		self.update()

	def paintEvent(self, _event):
		if self.track:
			pic = self.drawTrack()
			bounds = pic.boundingRect()
			corner = bounds.topLeft()
			scaling = min((self.width() - 40.0) / bounds.width(),
						  (self.height() - 40.0) / bounds.height())
			painter = QPainter(self)
			painter.scale(scaling, scaling)
			painter.drawPicture(-corner.x(), -corner.y(), pic)

	def remote_handle(self, type, msg):
		if type.text != "init":
			return
		key, value = msg
		if key.text == "sectors":
			self.track = [ self._buildSector(s) for s in value ]
			self.update()
