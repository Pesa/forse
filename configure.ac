AC_INIT(FORSE, [1.2])

AC_PREREQ([2.60])

AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL

AC_ERLANG_CHECK_LIB([kernel], [], [AC_MSG_ERROR([kernel not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [], [AC_MSG_ERROR([stdlib not found!])])
AC_ERLANG_CHECK_LIB([mnesia], [], [AC_MSG_ERROR([mnesia not found!])])

AX_PYTHON_PATH
AX_PYTHON_VERSION_ENSURE([2.6])

AX_PYTHON_MODULE([twisted])
AX_PYTHON_MODULE([twotp])

AX_PYTHON_MODULE([PyQt4.Qt])
AX_PYTHON_MODULE([PyQt4.QtCore])
AX_PYTHON_MODULE([PyQt4.QtGui])

AC_ARG_VAR([PYRCC], [PyQt4 resource compiler])
AC_PATH_PROG([PYRCC], pyrcc4)
AC_ARG_VAR([PYUIC], [PyQt4 user interface compiler])
AC_PATH_PROG([PYUIC], pyuic4)

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([src/forse/Makefile])
AC_CONFIG_FILES([environment])
AC_OUTPUT
