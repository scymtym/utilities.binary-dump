;;;; package.lisp --- Package definition for tests of the utilities.binary-dump system.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.binary-dump.test
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:nibbles
   #:fiveam

   #:utilities.binary-dump)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the utilities.binary-dump
    system."))

(cl:in-package #:utilities.binary-dump.test)

;;; Root test suite and external interface

(def-suite :utilities.binary-dump
  :description
  "Root unit test suite for the utilities.binary-dump system.")

(defun run-tests ()
  (run! :utilities.binary-dump))

;;; Test utilities

(defparameter *octets-1* (octet-vector 1 15 255 65))
