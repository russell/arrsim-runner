(defsystem :arrsim-runner
  :author "Russell Sim <russell.sim@gmail.com>"
  :version "0.1"
  :defsystem-depends-on (:asdf)
  :depends-on (:asdf
               :alexandria
               :fiveam
               :com.dvlsoft.clon
               #+sbcl :sb-posix
               #+sbcl :sb-bsd-sockets
               )
  :pathname "src/"
  :components ((:file "package")
               (:file "runner" :depends-on ("package"))))
