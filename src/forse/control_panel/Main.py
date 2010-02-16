import sys
from util import NodeApplication
from ControlPanel import ControlPanel


if __name__ == "__main__":
    app = NodeApplication("control_panel", False)
    mainwin = ControlPanel()
    mainwin.show()
    sys.exit(app.exec_())
