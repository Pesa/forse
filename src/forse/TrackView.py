from PyQt4.Qt import Qt
from PyQt4.QtGui import QGraphicsView, QPainter


__all__ = ['TrackView']


class TrackView(QGraphicsView):

    def __init__(self, parent=None):
        QGraphicsView.__init__(self, parent)
        self.setBackgroundBrush(Qt.lightGray)
        self.setRenderHint(QPainter.Antialiasing)
        self.setViewportUpdateMode(QGraphicsView.SmartViewportUpdate)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

    def refitSceneInView(self):
        self.fitInView(self.sceneRect(), Qt.KeepAspectRatio)

    def resizeEvent(self, event):
        self.refitSceneInView()
        QGraphicsView.resizeEvent(self, event)
