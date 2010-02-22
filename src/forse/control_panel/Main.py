import sys
from util import NodeApplication
from ControlPanelWindow import ControlPanelWindow


if __name__ == "__main__":
    app = NodeApplication("control_panel")
    mainwin = ControlPanelWindow()
    mainwin.show()
    sys.exit(app.exec_())
