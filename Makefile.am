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
	language/guilescript \
	guilescript

dist_bin_SCRIPTS = scripts/guilescript

ACLOCAL_AMFLAGS = -I build-aux

EXAMPLES = \
	examples/basic/basic-objects.gs \
	examples/basic/binary-search.gs \
	examples/basic/date.gs \
	examples/basic/fibonacci.gs \
	examples/basic/halve.gs \
	examples/basic/macro-add1.gs \
	examples/basic/macro-when.gs \
	examples/basic/timer.gs \
	examples/basic/vector-reverse.gs

TESTS = \
	tests/async.gs \
	tests/basics.gs \
	tests/control.gs \
	tests/error-association-list.gs \
	tests/error-await.gs \
	tests/error-dotted-pair.gs \
	tests/error-primitive-value.gs \
	tests/error-guile-module.gs \
	tests/error-guile-module-replace.gs \
	tests/error-guile-procedure.gs \
	tests/error-import.gs \
	tests/error-method.gs \
	tests/error-module-export.gs \
	tests/error-module-name.gs \
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
	tests/html.gs \
	tests/guile-modules.gs \
	tests/interop.gs \
	tests/lists.gs \
	tests/loops.gs \
	tests/modules.gs \
	tests/names.gs \
	tests/prelude.gs \
	tests/project.gs \
	tests/records.gs \
	tests/source-map.gs \
	tests/strings.gs \
	tests/unused.gs \
	tests/values.gs \
	tests/warning-await-for-each.gs \
	vite-plugin-guilescript/test.mjs \
	$(EXAMPLES)

TEST_EXTENSIONS = .gs .mjs
GS_LOG_COMPILER = $(top_srcdir)/tests/run-test
MJS_LOG_COMPILER = $(top_srcdir)/tests/run-node-test
AM_TESTS_ENVIRONMENT = \
	GUILE="$(GUILE)"; export GUILE; \
	GUILE_LOAD_COMPILED_PATH="$(abs_top_builddir)$${GUILE_LOAD_COMPILED_PATH:+:$$GUILE_LOAD_COMPILED_PATH}"; \
	export GUILE_LOAD_COMPILED_PATH;

EXTRA_DIST = \
	$(TESTS) \
	doc/reference.md \
	tests/project/greetings.gs \
	tests/project/shapes/circle.gs \
	tests/project/util.gs \
	tests/run-test \
	tests/run-node-test \
	vite-plugin-guilescript/README.md \
	vite-plugin-guilescript/index.js \
	vite-plugin-guilescript/package.json \
	examples/basic/fibonacci-server.scm \
	examples/basic/fibonacci-server.w \
	examples/task-manager/README.md \
	examples/task-manager/react/index.html \
	examples/task-manager/react/package-lock.json \
	examples/task-manager/react/package.json \
	examples/task-manager/react/src/app.gs \
	examples/task-manager/react/src/main.gs \
	examples/task-manager/react/vite.config.js \
	examples/task-manager/server.scm \
	examples/task-manager/shared/tasks/api.gs \
	examples/task-manager/shared/tasks/validation.gs \
	examples/task-manager/style.css \
	examples/task-manager/vanilla/app.gs \
	examples/task-manager/vanilla/dom.gs \
	examples/vite/README.md \
	examples/vite/index.html \
	examples/vite/package.json \
	examples/vite/src/counter.gs \
	examples/vite/src/main.gs \
	examples/vite/vite.config.js
