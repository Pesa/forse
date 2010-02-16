import sys
from util import NodeApplication
from LoggerWindow import LoggerWindow


if __name__ == "__main__":
    app = NodeApplication("debug_log")
    mainwin = LoggerWindow()
    mainwin.show()
    sys.exit(app.exec_())
