import sys
from util import NodeApplication
from NodeConfig import NodeConfig


if __name__ == "__main__":
    app = NodeApplication("node_configurator")
    dialog = NodeConfig()
    dialog.show()
    sys.exit(app.exec_())
