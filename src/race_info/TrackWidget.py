from PyQt4.Qt import Qt
from PyQt4.QtCore import pyqtSlot
from PyQt4.QtGui import QWidget, QPainter, QPen, QPicture
from Track import Track


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


class TrackWidget(QWidget):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.cars = [] # [ Car(i) for i in xrange(2, 10) ]
        self._track = None
#        self.totalLength = 0
#        for segment in self.track:
#            self.totalLength += len(segment)

#    def drawTrack(self):
#        trackPicture = QPicture()
#        painter = QPainter()
#        painter.begin(trackPicture)
#        painter.setRenderHint(QPainter.Antialiasing)
#        painter.rotate(90)
#        globalPos = 0
#        remaining = self.cars[:]
#        for segment in self.track:
#            segment.draw(painter, self.debug)
#            todo = remaining
#            remaining = []
#            for car in todo:
#                if car.position % self.totalLength < globalPos + len(segment):
#                    painter.save()
#                    segment.translate(painter, car.position % self.totalLength - globalPos)
#                    car.draw(painter)
#                    painter.restore()
#                else:
#                    remaining.append(car)
#            segment.finalize(painter)
#            globalPos += len(segment)
#        painter.end()
#        return trackPicture

    @pyqtSlot()
    def moveCars(self):
        for car in self.cars:
            car.advance()
        self.update()

    def paintEvent(self, _event):
        if self._track:
            pic = self._track.draw()
            bounds = pic.boundingRect()
            scaling = min(float(self.width()) / bounds.width(),
                          float(self.height()) / bounds.height())
            x = abs(bounds.x()) + (float(self.width()) / scaling - bounds.width()) / 2
            y = abs(bounds.y()) + (float(self.height()) / scaling - bounds.height()) / 2
            painter = QPainter(self)
            painter.scale(scaling, scaling)
            painter.drawPicture(x, y, pic)

    def remote_handle(self, type, msg):
        if type.text != "init":
            return
        key, value = msg
        if key.text == "sectors":
            self._track = Track(value)
            self.update()
