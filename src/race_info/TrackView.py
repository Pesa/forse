from PyQt4.Qt import Qt
from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QGraphicsScene, QGraphicsView, QPainter
from Car import Car
from Track import Track


class TrackView(QGraphicsView):

    def __init__(self, parent=None):
        self._scene = QGraphicsScene()
        QGraphicsView.__init__(self, self._scene, parent)
        self.setCacheMode(QGraphicsView.CacheBackground)
        self.setRenderHint(QPainter.Antialiasing)
        self.setBackgroundBrush(Qt.lightGray)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self._cars = {}
        self._track = None
        self._timer = QTimer()
        self._timer.timeout.connect(self._scene.advance)

    def _refitScene(self):
        self.fitInView(self.sceneRect(), Qt.KeepAspectRatio)

    def resizeEvent(self, _event):
        self._refitScene()

    def remote_handle(self, type, msg):
        if type.text == "init":
            key, value = msg
            if key.text == "cars_pos":
                for id, pos in value:
                    car = Car(self._track, id, pos)
                    self._cars[id] = car
                    self._scene.addItem(car)
            elif key.text == "race_state":
                if value.text == "started" or value.text == "resumed":
                    self._timer.start(40)
                else:
                    self._timer.stop()
            elif key.text == "sectors":
                self._track = Track(value)
                self._scene.addItem(self._track)
                self._refitScene()
        elif type.text == "update":
            key, id, pos = msg
            if key.text == "car_pos":
                self._cars[id].position = pos
