import OTPApplication
from PyQt4.Qt import Qt
from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QGraphicsScene, QGraphicsView, QPainter
from Car import Car
from Track import Track
from Util import atomToBool


class TrackView(QGraphicsView):

    def __init__(self, parent=None):
        self._scene = QGraphicsScene()
        self._scene.setItemIndexMethod(QGraphicsScene.NoIndex)
        QGraphicsView.__init__(self, self._scene, parent)
        self.setBackgroundBrush(Qt.lightGray)
        self.setRenderHint(QPainter.Antialiasing)
        self.setViewportUpdateMode(QGraphicsView.SmartViewportUpdate)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self._cars = {}
        self._track = None
        self._timer = QTimer(self)
        self._timer.setInterval(40)
        self._timer.timeout.connect(self._scene.advance)
        handlers = {('init', 'cars_pos'): self._initCars,
                    ('init', 'sectors'): self._initTrack,
                    ('init', 'race_state'): self._setRaceState,
                    ('update', 'car_pos'): self._moveCar}
        OTPApplication.registerMsgHandlers(handlers)

    def resizeEvent(self, _event):
        self._refitScene()

    def _initCars(self, cars):
        for id, pos, pit in cars:
            car = Car(self._track, id, pos, atomToBool(pit))
            self._cars[id] = car
            self._scene.addItem(car)

    def _initTrack(self, track):
        self._track = Track(track)
        self._scene.addItem(self._track)
        self._refitScene()

    def _moveCar(self, value):
        id, pos, pit = value
        self._cars[id].updatePos(pos, atomToBool(pit))

    def _refitScene(self):
        self.fitInView(self.sceneRect(), Qt.KeepAspectRatio)

    def _setRaceState(self, state):
        if state.text == "started" or state.text == "resumed":
            self._timer.start()
        else:
            self._timer.stop()
