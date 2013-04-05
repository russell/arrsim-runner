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
  (group (:header "Debugging options:")
         (flag :short-name "t" :long-name "trace"
               :description "Trace function calls."))
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit.")))

(defun safe-trace (sym package)
  (handler-bind ((simple-error
                   #'(lambda (c)
                       (format t "Skipping Symbol ~A (probably a macro)~%" sym)
                       (invoke-restart 'skip-symbol))))
    (if (and (fboundp sym)
             (functionp (symbol-function sym))
             (equal (package-name (symbol-package sym))
                    (symbol-name package)))
        (progn
          (format t "Tracing symbol ~A~%" sym)
          (restart-case
              (eval `(trace ,(intern (symbol-name sym) package)))
            (skip-symbol () nil)))
        (format t "Not tracing Symbol ~A~%" sym))))

(defun trace-package (package)
  (do-symbols (sym package)
    (safe-trace sym package)))

(defun main (&rest arguments)
  (make-context :cmdline arguments)
  (flet ((considered-exit () (help) (exit)))
    (when (getopt :short-name "h") (considered-exit))
    (cond
      ((> (length (remainder)) 1)
       (format t "ERROR, only one system allowed.~%")
       (considered-exit))
      ((< (length (remainder)) 1)
       (format t "ERROR, no system found.  At least one must be specified.~%")
       (considered-exit))))
  (let ((package (intern (string-upcase (car (remainder))))))
    (asdf:oos 'asdf:load-op package)
    (when (getopt :short-name "t")
      (trace-package package))
    (asdf:oos 'asdf:test-op package)))

(defun entry-point ()
  (apply 'main "test-op" asdf/image:*command-line-arguments*))
