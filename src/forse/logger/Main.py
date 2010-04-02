import sys
from LoggerWindow import LoggerWindow
from Subscriber import SubscriberApplication


if __name__ == "__main__":
    app = SubscriberApplication("debug_log")
    mainwin = LoggerWindow()
    mainwin.show()
    app.subscribe()
    sys.exit(app.exec_())
