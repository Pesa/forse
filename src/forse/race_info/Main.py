import sys
from util import SubscriberApplication
from RaceInfoWindow import RaceInfoWindow


if __name__ == "__main__":
    app = SubscriberApplication("race_info")
    mainwin = RaceInfoWindow()
    mainwin.show()
    sys.exit(app.exec_())
