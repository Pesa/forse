###########################################################################
#
# Copyright (c) 2010  Davide Pesavento <davidepesa@gmail.com>
#
# This file is part of FORSE.
#
# FORSE is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# FORSE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with FORSE.  If not, see <http://www.gnu.org/licenses/>.
#
###########################################################################

import hashlib, random, socket
from twotp import Atom
from PyQt4.QtCore import QString


__all__ = ['atomToBool', 'listToString', 'mpsToString',
           'secondsToString', 'buildNodeName', 'randomHash']


def atomToBool(atom):
    """
    Translates an Erlang atom to a Python boolean value.
    Raises an exception if the conversion fails.
    """
    if not isinstance(atom, Atom):
        raise TypeError(str(atom) + " is not an atom.")
    if atom.text == "true":
        return True
    elif atom.text == "false":
        return False
    else:
        raise ValueError(str(atom) + " is not a valid boolean value.")


def listToString(l):
    """
    Translates an Erlang string (a list of character codes) to a QString.
    """
    if not isinstance(l, list):
        raise TypeError(str(l) + " is not a list.")
    return QString.fromUtf8(''.join([chr(c) for c in l]))


def mpsToString(mps):
    """
    Converts a speed value (expressed in m/s) to Km/h
    and returns it as a nicely-formatted string.
    """
    return "%.2f Km/h" % (mps * 3.6)


def secondsToString(seconds):
    """
    Converts a time value (expressed in seconds) to
    a user-friendly format and returns it as a string.
    """
    x = int(seconds)
    msecs = round((seconds - x) * 1000)
    return "%i:%02i.%03i" % (x / 60, x % 60, msecs)


def buildNodeName(name, randomize=False):
    """
    Returns a string that can be used as the name of an Erlang or Python node.
    If C{randomize} is true, a random hexadecimal hash is appended to C{name}.
    """
    if not name.startswith("forse_"):
        name = "forse_" + name
    if randomize:
        name += '_' + randomHash()
    if '@' not in name:
        name += '@' + socket.gethostname()
    return name


def randomHash(length=8):
    """
    Generates a random hexadecimal string of the specified length.
    """
    return hashlib.sha1(str(random.random())).hexdigest()[:length]
