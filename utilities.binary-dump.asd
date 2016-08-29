;;;; utilities.binary-dump.asd --- System definition for utilities.binary-dump.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.binary-dump-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:utilities.binary-dump-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definition

(defsystem :utilities.binary-dump
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Formatting of binary data similar to the od(1) UNIX program."
  :depends-on  (:alexandria
                :let-plus
                :nibbles)
  :encoding    :utf-8
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "access")
                              (:file       "formatting")))

                (:static-file "README.org"))

  :in-order-to ((test-op (test-op :utilities.binary-dump-test))))

(defsystem :utilities.binary-dump-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the utilities.binary-dump system."
  :depends-on  (:alexandria
                :split-sequence
                :let-plus
                :nibbles
                (:version :utilities.binary-dump #.(version/string))
                (:version :fiveam                "1.3"))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "access")
                              (:file       "formatting")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :utilities.binary-dump-test))))
  (funcall (read-from-string "utilities.binary-dump.test:run-tests")))
