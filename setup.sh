#!/bin/bash

# Assuming you have QuickLisp installed, you can install the required packages with
QL="/usr/lib/quicklisp/setup.lisp"
sbcl --load "$QL" \
     --eval "(ql:quickload uiop)" \
     --eval "(ql:quickload cl-utilities)" \
     --eval "(ql:quickload mito)" \
     --quit

