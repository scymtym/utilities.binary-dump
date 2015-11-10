;;;; formatting.lisp --- Unit tests for formatting functions.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.binary-dump.test)

(in-suite :utilities.binary-dump)

(test map-units.smoke
  "Smoke test for the `map-units' function."

  (mapc
   (lambda+ ((data args expected))
     (let+ ((units '())
            ((&flet collect (unit last?)
               (push (cons unit last?) units))))
       (apply #'map-units #'collect data args)
       (is (equal expected (reverse units)))))

   `((,*octets-1* ( 8 :little unsigned-byte)
                  ((1) (15) (255) (65 . t)))
     (,*octets-1* ( 8 :big    unsigned-byte)
                  ((1) (15) (255) (65 . t)))
     (,*octets-1* (16 :little unsigned-byte)
                  ((3841) (16895 . t)))
     (,*octets-1* (16 :big    unsigned-byte)
                  ((271) (65345 . t))))))

(test map-chunks.smoke
  "Smoke test for the `map-chunks' function."

  (mapc
   (lambda+ ((data length args expected-chunks expected-values))
     (let+ ((chunks '())
            ((&flet collect (&rest args)
               (push args chunks)))
            (values (multiple-value-list
                     (apply #'map-chunks #'collect data length args))))
       (is (equal expected-chunks (reverse chunks)))
       (is (equal expected-values values))))

   `((,*octets-1* 1 ()              ((0 ,*octets-1* 0 1 nil)
                                     (1 ,*octets-1* 1 2 nil)
                                     (2 ,*octets-1* 2 3 nil)
                                     (3 ,*octets-1* 3 4 t))
                                    (,*octets-1* 0 4 4))
     (,*octets-1* 2 ()              ((0 ,*octets-1* 0 2 nil)
                                     (2 ,*octets-1* 2 4 t))
                                    (,*octets-1* 0 4 2))
     (,*octets-1* 4 ()              ((0 ,*octets-1* 0 4 t))
                                    (,*octets-1* 0 4 1))
     (,*octets-1* 8 ()              ((0 ,*octets-1* 0 4 t))
                                    (,*octets-1* 0 4 1))
     (,*octets-1* 1 (:max-chunks 1) ((0 ,*octets-1* 0 1 t))
                                    (,*octets-1* 0 1 1)))))

(macrolet
    ((define-conditions-case (function &rest required-args)
       (let ((case-name (symbolicate function '#:.conditions)))
         `(test ,case-name
            (let+ (((&flet do-it (data &rest args)
                      (apply #',function (constantly nil) data
                             ,@required-args args))))
              (signals error
                (do-it (octet-vector 1 2) :start 1 :end 0))
              (signals error
                (do-it (octet-vector 1 2) :start 3))
              (signals error
                (do-it (octet-vector 1 2) :end 3)))))))

  (define-conditions-case map-units 8 :big 'unsigned-byte)
  (define-conditions-case map-chunks 8))
