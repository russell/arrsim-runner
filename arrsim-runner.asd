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
               #+sbcl :sb-cover
               )
  :pathname "src/"
  :components ((:file "package")
               (:file "runner" :depends-on ("package"))))
