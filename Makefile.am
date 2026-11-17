#
# Makefile.am
#
# Copyright (C) 2022-2026 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of GuileScript.
#
# GuileScript is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# GuileScript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GuileScript. If not, see https://www.gnu.org/licenses/.
#

SUBDIRS = \
	language/javascript \
	language/guilescript

dist_bin_SCRIPTS = guilescript

ACLOCAL_AMFLAGS = -I build-aux

EXAMPLES = \
	examples/basic-objects.gs \
	examples/binary-search.gs \
	examples/date.gs \
	examples/fibonacci.gs \
	examples/halve.gs \
	examples/macro-add1.gs \
	examples/macro-when.gs \
	examples/timer.gs \
	examples/vector-reverse.gs

TESTS = \
	tests/async.gs \
	tests/basics.gs \
	tests/control.gs \
	tests/error-await.gs \
	tests/error-define-module.gs \
	tests/error-dotted-pair.gs \
	tests/error-guile-module.gs \
	tests/error-guile-module-replace.gs \
	tests/error-guile-procedure.gs \
	tests/error-import.gs \
	tests/error-method.gs \
	tests/error-name-clash.gs \
	tests/error-optional-key.gs \
	tests/error-parameterize.gs \
	tests/error-property.gs \
	tests/error-record-arity.gs \
	tests/error-setter.gs \
	tests/error-undeclared.gs \
	tests/functions.gs \
	tests/globals.gs \
	tests/guard.gs \
	tests/guile-modules.gs \
	tests/interop.gs \
	tests/lists.gs \
	tests/loops.gs \
	tests/modules.gs \
	tests/names.gs \
	tests/prelude.gs \
	tests/records.gs \
	tests/strings.gs \
	$(EXAMPLES)

TEST_EXTENSIONS = .gs
GS_LOG_COMPILER = $(top_srcdir)/tests/run-test
AM_TESTS_ENVIRONMENT = \
	GUILE="$(GUILE)"; export GUILE; \
	GUILE_LOAD_COMPILED_PATH="$(abs_top_builddir)$${GUILE_LOAD_COMPILED_PATH:+:$$GUILE_LOAD_COMPILED_PATH}"; \
	export GUILE_LOAD_COMPILED_PATH;

EXTRA_DIST = \
	$(TESTS) \
	tests/run-test \
	examples/fibonacci-server.scm
