import sys
from util import SubscriberApplication
from LoggerWindow import LoggerWindow


if __name__ == "__main__":
    app = SubscriberApplication("debug_log")
    mainwin = LoggerWindow()
    mainwin.show()
    sys.exit(app.exec_())
