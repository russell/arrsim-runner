
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
