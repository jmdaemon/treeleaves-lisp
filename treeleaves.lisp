#!/usr/bin/sbcl --script

(require "uiop")

(defparameter *dirs* nil "All recursive directories.")

(uiop:collect-sub*directories "~/Documents"
    (constantly t)
    (constantly t)
    (lambda (it) (push it *dirs*)))
(print *dirs*)
