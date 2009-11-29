from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QWidget, QPainter
from Car import Car
from Track import Track


class TrackWidget(QWidget):

    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self._cars = {}
        self._track = None
        self._timer = QTimer()
        self._timer.timeout.connect(self.update)

    def paintEvent(self, _event):
        if not self._track:
            return
        pic = self._track.draw(self._cars.values())
        bounds = pic.boundingRect()
        scaling = min(float(self.width()) / bounds.width(),
                      float(self.height()) / bounds.height())
        x = abs(bounds.x()) + (float(self.width()) / scaling - bounds.width()) / 2
        y = abs(bounds.y()) + (float(self.height()) / scaling - bounds.height()) / 2
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)
        painter.scale(scaling, scaling)
        painter.drawPicture(x, y, pic)

    def remote_handle(self, type, msg):
        if type.text == "init":
            key, value = msg
            if key.text == "cars_pos":
                for id, pos in value:
                    self._cars[id] = Car(id, pos)
                self.update()
            elif key.text == "race_state":
                if value.text == "started" or value.text == "resumed":
                    print "starting timer"
                    self._timer.start(50)
                else:
                    print "stopping timer"
                    self._timer.stop()
            elif key.text == "sectors":
                self._track = Track(value)
                self.update()
        elif type.text == "update":
            a, b, c = msg
            if a.text == "car_pos":
                self._cars[b].position = c
