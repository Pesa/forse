import hashlib, random, socket
from twotp.term import Atom


__all__ = ['atomToBool', 'listToString', 'buildNodeName', 'randomHash']


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
    Translates an Erlang string (a list of ASCII codes) to a Python string.
    """
    if not isinstance(l, list):
        raise TypeError(str(l) + " is not a list.")
    return ''.join([ chr(c) for c in l ])


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
