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
               :description "Trace function calls.")
         (lispobj :short-name "c" :long-name "coverage"
               :description "Generate a coverage report."))
  (group (:header "FiveAM:")
         (lispobj :long-name "fiveam"
               :description "package to load.")
         (lispobj :short-name "p" :long-name "package"
               :description "package to load."))
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit.")))


(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defvar *pwd* (pathname (concatenate 'string (getenv "PWD") "/")))

(defun enable-coverage ()
  #+sbcl
  (progn
    (require 'sb-cover)
    (declaim (optimize sb-cover:store-coverage-data))))

(defun write-coverage ()
  (let ((dir (merge-pathnames "coverage/" *pwd*)))
    #+sbcl
    (progn
      (format t "writing coverage output to ~A~%" dir)
      (sb-cover:report dir))))

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

(defgeneric run-tests (runner package))

(defmethod run-tests ((runner (eql :fiveam)) package)
  (dolist (result (run! package))
    (print result)))

(defmethod run-tests ((runner (eql :asdf)) package)
  (asdf:oos 'asdf:test-op package))

(defun main (&rest arguments)
  (make-context :cmdline arguments)
  (flet ((considered-exit () (help) (exit)))
    (when (getopt :short-name "h") (considered-exit)))
  (let ((package (getopt :short-name "p"))
        (coverage (getopt :short-name "c"))
        (fiveam (getopt :long-name "fiveam")))
    ;; load package
    (asdf:oos 'asdf:load-op package)
    (when coverage
      (enable-coverage)
      (asdf:oos 'asdf:load-op coverage :force t)
      (asdf:oos 'asdf:load-op package :force t))
    (when (getopt :short-name "t")
      (trace-package package))
    ;; run tests
    (when fiveam
      (run-tests :fiveam (intern (symbol-name fiveam) (find-package 'keyword))))
    (when coverage
      (write-coverage))))

(defun entry-point ()
  (apply 'main "arrsim-runner" asdf/image:*command-line-arguments*))
