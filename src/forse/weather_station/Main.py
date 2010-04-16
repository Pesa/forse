import sys
from Subscriber import SubscriberApplication
from WeatherStationWindow import WeatherStationWindow


if __name__ == "__main__":
    app = SubscriberApplication("weather")
    mainwin = WeatherStationWindow()
    mainwin.show()
    sys.exit(app.exec_())
