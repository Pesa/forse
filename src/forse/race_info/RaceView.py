###########################################################################
#
# Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
#
# This file is part of FORSE.
#
# FORSE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# FORSE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
#
###########################################################################

from PyQt4.Qt import Qt
from PyQt4.QtCore import QTimer
from PyQt4.QtGui import QGraphicsScene
from OTPApplication import OTPApplication
from Car import Car
from Track import Track
from TrackView import TrackView
from Util import atomToBool


class RaceView(TrackView):

    def __init__(self, parent=None):
        TrackView.__init__(self, parent)
        scene = QGraphicsScene()
        scene.setItemIndexMethod(QGraphicsScene.NoIndex)
        self.setScene(scene)
        self._cars = {}
        self._track = None
        self._timer = QTimer(self)
        self._timer.setInterval(40)
        self._timer.timeout.connect(self.scene().advance)
        handlers = {('init', 'cars_pos'): self._initCars,
                    ('init', 'sectors'): self._initTrack,
                    ('init', 'race_state'): self._setRaceState,
                    ('update', 'cars_pos'): self._moveCars}
        OTPApplication.registerMsgHandlers(handlers)

    def reloadPilotInfo(self):
        for car in self._cars.itervalues():
            car.refreshState()

    def _color(self):
        color = 5
        while True:
            yield Qt.GlobalColor(color + 7)
            color = (color + 1) % 12

    def _initCars(self, cars):
        for carId, pos, pit in cars:
            c = Car(self._track, carId, pos, atomToBool(pit))
            self._cars[carId] = c
            self.scene().addItem(c)

    def _initTrack(self, sectors):
        self._track = Track(sectors, self._color().next)
        self.scene().addItem(self._track)
        self.refitSceneInView()

    def _moveCars(self, cars):
        for carId, pos, pit in cars:
            self._cars[carId].updatePos(pos, atomToBool(pit))

    def _setRaceState(self, state):
        if state.text == "running":
            self._timer.start()
        else:
            self._timer.stop()
