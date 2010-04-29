from PyQt4.Qt import Qt
from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QGraphicsScene, QGraphicsView, QPainter
from OTPApplication import OTPApplication
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
                    ('update', 'cars_pos'): self._moveCars}
        OTPApplication.registerMsgHandlers(handlers)

    def reloadPilotInfo(self):
        for car in self._cars.itervalues():
            car.refreshToolTip()

    def resizeEvent(self, _event):
        self._refitScene()

    def _color(self):
        color = 5
        while True:
            yield Qt.GlobalColor(color + 7)
            color = (color + 1) % 12

    def _initCars(self, cars):
        for carId, pos, pit in cars:
            c = Car(self._track, carId, pos, atomToBool(pit))
            self._cars[carId] = c
            self._scene.addItem(c)

    def _initTrack(self, sectors):
        self._track = Track(sectors, self._color().next)
        self._scene.addItem(self._track)
        self._refitScene()

    def _moveCars(self, cars):
        for carId, pos, pit in cars:
            self._cars[carId].updatePos(pos, atomToBool(pit))

    def _refitScene(self):
        self.fitInView(self.sceneRect(), Qt.KeepAspectRatio)

    def _setRaceState(self, state):
        if state.text == "running":
            self._timer.start()
        else:
            self._timer.stop()
