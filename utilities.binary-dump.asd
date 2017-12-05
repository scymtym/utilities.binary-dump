;;;; utilities.binary-dump.asd --- System definition for utilities.binary-dump.
;;;;
;;;; Copyright (C) 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :utilities.binary-dump
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
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

  :in-order-to ((test-op (test-op :utilities.binary-dump/test))))

(defsystem :utilities.binary-dump/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the utilities.binary-dump system."
  :depends-on  (:alexandria
                :split-sequence
                :let-plus
                :nibbles

                (:version :fiveam                "1.3")

                (:version :utilities.binary-dump (:read-file-form "version-string.sexp")))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "access")
                              (:file       "formatting")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :utilities.binary-dump/test))))
  (uiop:symbol-call '#:utilities.binary-dump.test '#:run-tests))
