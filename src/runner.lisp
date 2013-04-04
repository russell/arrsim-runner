;; arrsim-runner is a simple test runner for common lisp
;; Copyright (C) 2013 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package :arrsim-runner)

(defsynopsis (:postfix "SYSTEMS")
  (text :contents "Run the tests for a common lisp library.")
  (group (:header "Immediate exit options:")
    (flag :short-name "h" :long-name "help"
      :description "Print this help and exit.")
    (flag :short-name "v" :long-name "version"
          :description "Print version number and exit.")))

(defun main (&rest arguments)
  (make-context :cmdline arguments)
  (when (getopt :short-name "h")
    (help)
    (exit))
  (when (> (length (remainder)) 1)
    (format t "Error, only one system allowed.~%")
    (help)
    (exit))
  (asdf:oos 'asdf:test-op (intern (string-upcase (car (remainder))) "KEYWORD")))

(defun entry-point ()
  (apply 'main "test-op" asdf/image:*command-line-arguments*))
