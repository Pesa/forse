import sys
from RaceInfoWindow import RaceInfoWindow
from Subscriber import SubscriberApplication


if __name__ == "__main__":
    app = SubscriberApplication("race_info")
    mainwin = RaceInfoWindow()
    mainwin.show()
    app.subscribe()
    sys.exit(app.exec_())
