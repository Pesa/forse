import sys
from NodeConfig import NodeConfig
from OTPApplication import OTPApplication


if __name__ == "__main__":
    app = OTPApplication("node_configurator")
    dialog = NodeConfig()
    dialog.show()
    sys.exit(app.exec_())
