import sys
from util import NodeApplication
from RaceInfoWindow import RaceInfoWindow


if __name__ == "__main__":
    app = NodeApplication("race_info")
    mainwin = RaceInfoWindow()
    mainwin.show()
    sys.exit(app.exec_())
