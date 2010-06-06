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
from PyQt4.QtCore import pyqtSignal
from PyQt4.QtGui import QColor, QGraphicsScene
from OTPApplication import OTPApplication
from Track import PhysicalSector, Track
from TrackView import TrackView


class WeatherView(TrackView):

    sectorClicked = pyqtSignal(int)

    def __init__(self, parent=None):
        TrackView.__init__(self, parent)
        self.setScene(QGraphicsScene())
        self._track = None
        handlers = {('init', 'sectors'): self._initTrack,
                    ('init', 'weather'): self._setWeather,
                    ('update', 'weather'): self._setWeather}
        OTPApplication.registerMsgHandlers(handlers)

    def mousePressEvent(self, event):
        handled = False
        for item in self.items(event.pos()):
            if isinstance(item, PhysicalSector):
                sectId = item.sectorId()
                if sectId is not None:
                    self.sectorClicked.emit(sectId)
                    handled = True
                break
        if not handled:
            TrackView.mousePressEvent(self, event)

    def _initTrack(self, sectors):
        self._track = Track(sectors, lambda: Qt.darkGray)
        self._track.enableHoverEffect()
        self.scene().addItem(self._track)
        self.refitSceneInView()

    def _setWeather(self, weather):
        for sectId, rain in weather:
            #rain = sectId % 11 # FIXME: only for testing
            r = g = 100 - rain * 10
            b = 50 + (360 / (12 - rain))
            self._track.setSectorColor(sectId, QColor(r, g, b))
            self._track.setSectorToolTip(sectId, "Amount of rain: %i" % rain)
