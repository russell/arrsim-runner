PREFIX	:= /usr/local

PROGRAMS	:= test-op

QUICKLISP_LOCATION := "(merge-pathnames \".quicklisp/setup.lisp\" (user-homedir-pathname))"

QUICKLISP_INIT := "(let ((quicklisp-init "${QUICKLISP_LOCATION}"))" \
  "(when (probe-file quicklisp-init)" \
    "(load quicklisp-init)))"

CL_LAUNCH ?= cl-launch
CL_LAUNCH_STANDALONE := nil

LISP ?= sbcl
CL_LAUNCH_FLAGS ?=
CL_LAUNCH_FLAGS += --lisp '${LISP} sbcl clisp ccl'
CL_LAUNCH_FLAGS += --no-include

INSTALL_BIN ?= ${PREFIX}/bin
INSTALL_IMAGE ?= ${PREFIX}/lib/common-lisp/images

all: $(PROGRAMS)

test-op:
	echo ${QUICKLISP_INIT} > .quicklisp-init.lisp
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} --file .quicklisp-init.lisp --system arrsim-runner --restart "arrsim-runner:entry-point" --output ${INSTALL_BIN}/test-op
