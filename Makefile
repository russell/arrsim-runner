# arrsim-runner is a simple test runner for common lisp
# Copyright (C) 2013 Russell Sim <russell.sim@gmail.com>
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

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
	${CL_LAUNCH} ${CL_LAUNCH_FLAGS} \
		--file .quicklisp-init.lisp \
		--system arrsim-runner \
		--restart "arrsim-runner:entry-point" \
		--output ${INSTALL_BIN}/arrsim-runner
		# cl-launch defaults to executable so image mode wont work.
	    # --dump ${INSTALL_IMAGE}/test-op.image
