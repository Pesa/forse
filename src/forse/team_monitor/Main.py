import sys
from Subscriber import SubscriberApplication
from TeamMonitorWindow import TeamMonitorWindow


if __name__ == "__main__":
    app = SubscriberApplication("team")
    mainwin = TeamMonitorWindow()
    mainwin.show()
    sys.exit(app.exec_())
