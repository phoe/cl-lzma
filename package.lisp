;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-LZMA
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:cl-lzma/autowrap
    (:use)
  (:export #:cl-lzma/autowrap
           #:make-i-sz-alloc
           #:lzma-enc-props-init
           #:c-lzma-enc-props.dict-size
           #:c-lzma-enc-props.fb
           #:lzma-encode
           #:lzma-decode
           #:i-sz-alloc.alloc
           #:i-sz-alloc.free
           #:c-lzma-enc-props
           #:e-lzma-status
           #:+sz-ok+
           #:+lzma-finish-end+))

(defpackage #:cl-lzma
  (:use #:cl
        #:autowrap)
  (:import-from #:cffi
                #:define-foreign-library
                #:use-foreign-library
                #:foreign-array-to-lisp
                #:mem-ref
                #:foreign-alloc
                #:foreign-free)
  (:import-from #:cl-lzma/autowrap
                #:make-i-sz-alloc
                #:lzma-enc-props-init
                #:c-lzma-enc-props.dict-size
                #:c-lzma-enc-props.fb
                #:lzma-encode
                #:lzma-decode
                #:i-sz-alloc.alloc
                #:i-sz-alloc.free
                #:c-lzma-enc-props
                #:e-lzma-status
                #:+sz-ok+
                #:+lzma-finish-end+)
  (:export #:lzma-compress
           #:lzma-decompress
           #:lzma-test))
