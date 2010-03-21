import sys
from ControlPanelWindow import ControlPanelWindow
from OTPApplication import OTPApplication


if __name__ == "__main__":
    app = OTPApplication("control_panel")
    app.setQuitOnLastWindowClosed(False)
    mainwin = ControlPanelWindow()
    mainwin.show()
    sys.exit(app.exec_())
