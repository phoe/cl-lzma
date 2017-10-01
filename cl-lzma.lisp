;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-LZMA
;;;; © Michał "phoe" Herda 2017
;;;; cl-lzma.lisp

(in-package #:cl-lzma)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FFI

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline system-relative-namestring))
  (defun system-relative-namestring (system name)
    (namestring (asdf:system-relative-pathname system name))))

(cffi:define-foreign-library lzma
  (:darwin #.(system-relative-namestring
              :cl-lzma
              #+x86 "bin/lzma-mac32.dylib"
              #+x86-64 "bin/lzma-mac64.dylib"))
  (:unix #.(system-relative-namestring
            :cl-lzma
            #+x86 "bin/lzma-lin32.so"
            #+x86-64 "bin/lzma-lin64.so"))
  (:windows #.(system-relative-namestring
               :cl-lzma
               #+x86 "bin/lzma-win32.dll"
               #+x86-64 "bin/lzma-win64.dll"))
  (t (:default "lzma")))

(cffi:use-foreign-library lzma)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-Autowrap

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *spec-path*
    (asdf:system-relative-pathname :cl-lzma "spec/"))
  (c-include (merge-pathnames "lzma.h" *spec-path*)
             :spec-path *spec-path*
             :definition-package :cl-lzma/autowrap
             :function-package :cl-lzma/autowrap
             :wrapper-package :cl-lzma/autowrap
             :accessor-package :cl-lzma/autowrap
             :constant-package :cl-lzma/autowrap
             :extern-package :cl-lzma/autowrap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; C callbacks

(defcallback lzma-alloc :pointer ((allocptr :pointer) (size size-t))
  (declare (ignore allocptr))
  (foreign-alloc :char :count size))

(defcallback lzma-free :void ((allocptr :pointer) (address :pointer))
  (declare (ignore allocptr))
  (unless (cffi:null-pointer-p address)
    (foreign-free address)))

(defvar *alloc-functions*
  (let* ((ptr (cffi:foreign-alloc :pointer :count 2))
         (struct (make-i-sz-alloc :ptr ptr)))
    (setf (i-sz-alloc.alloc struct) (autowrap:callback 'lzma-alloc)
          (i-sz-alloc.free struct) (autowrap:callback 'lzma-free))
    struct))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions

(defun lzma-compress (vector)
  (check-type vector vector)
  (let ((dest-len (truncate (max 1024 (* (length vector) 1.5)))))
    (cffi:with-foreign-array
        (src vector `(:array :unsigned-char ,(length vector)))
      (with-many-alloc ((dest :unsigned-char dest-len)
                        (dest-len-ptr :unsigned-int 1))
        (setf (cffi:mem-ref dest-len-ptr :unsigned-int) dest-len)
        (%lzma-compress dest dest-len-ptr src (length vector))))))

(defun %lzma-compress (dest dest-len src src-len)
  (flet ((byte-array (length)  `(:array :uint8 ,length))
         (init-props (props input-size)
           (let ((dict-size (min input-size (expt 2 20))))
             (lzma-enc-props-init (c-lzma-enc-props-ptr props))
             (setf (c-lzma-enc-props.dict-size props) dict-size
                   (c-lzma-enc-props.fb props) 40))))
    (with-many-alloc ((props 'c-lzma-enc-props 1)
                      (props-size :unsigned-int 1)
                      (props-encoded :unsigned-char 5))
      (init-props props src-len)
      (setf (mem-ref props-size :unsigned-int) 5)
      (let ((status (lzma-encode dest dest-len src src-len
                                 props props-encoded props-size
                                 0 (cffi:null-pointer)
                                 (i-sz-alloc-ptr *alloc-functions*)
                                 (i-sz-alloc-ptr *alloc-functions*))))
        (unless (= status +sz-ok+)
          (error "LZMA compression failed with code ~D." status))
        (let* ((output-length (mem-ref dest-len :unsigned-int))
               (dest (foreign-array-to-lisp dest (byte-array output-length)))
               (props (foreign-array-to-lisp props-encoded (byte-array 5)))
               (dest (coerce dest '(vector (unsigned-byte 8))))
               (props (coerce props '(vector (unsigned-byte 8)))))
          (values dest props src-len))))))

(defun lzma-decompress (vector props-encoded unc-len)
  (check-type vector vector)
  (assert (< unc-len (* 256 1024 1024)))
  (cffi:with-foreign-array (src vector `(:array :unsigned-char
                                                ,(length vector)))
    (cffi:with-foreign-array (props props-encoded '(:array :unsigned-char 5))
      (with-many-alloc ((e-lzma-status 'e-lzma-status 1)
                        (proc-out-size :unsigned-long 1)
                        (proc-in-size :unsigned-long 1)
                        (dest :unsigned-char unc-len))
        (setf (mem-ref proc-out-size :unsigned-long) unc-len
              (mem-ref proc-in-size :unsigned-long) (length vector))
        (let ((status (lzma-decode dest proc-out-size src proc-in-size
                                   props 5 +lzma-finish-end+
                                   e-lzma-status
                                   (i-sz-alloc-ptr *alloc-functions*))))
          (let ((act-len (mem-ref proc-out-size :unsigned-int)))
            (unless (= status +sz-ok+)
              (error "LZMA compression failed with code ~D." status))
            (unless (= unc-len act-len)
              (error "Expected to uncompress ~D bytes, but got ~D bytes."
                     unc-len act-len))
            (coerce (foreign-array-to-lisp dest
                                           `(:array :unsigned-char ,unc-len))
                    '(vector (unsigned-byte 8)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Documentation

(setf (documentation 'lzma-compress 'function)
      "Compresses the provided vector using LZMA compression.
Arguments:
  * uncompressed vector, containing only elements of type (unsigned-byte 8).
Return values:
  * the resulting byte vector of type (vector (unsigned-byte 8) *),
  * the encoded LZMA properties of type (vector (unsigned-byte 8) 5),
  * the original size of uncompressed data - a nonnegative integer.")

(setf (documentation 'lzma-decompress 'function)
      "Decompresses the provided data using LZMA decompression.
Arguments:
  * the resulting byte vector of type (vector (unsigned-byte 8) *),
  * the encoded LZMA properties of type (vector (unsigned-byte 8) 5),
  * the original size of uncompressed data - a nonnegative integer.
Return values:
  * uncompressed vector, containing only elements of type (unsigned-byte 8).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tests

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun random-vector (length)
    (let ((vector (make-array length :element-type '(unsigned-byte 8)))
          (*random-state* (make-random-state t)))
      (loop for i below (length vector)
            do (setf (aref vector i) (random 256)))
      vector)))

(defparameter *lzma-test-input*
  `(,(make-array 1000 :element-type '(unsigned-byte 8))
    ,(make-array 1000000 :element-type '(unsigned-byte 8))
    ,(make-array 1000 :element-type '(unsigned-byte 8)
                      :initial-element 255)
    ,(make-array 1000000 :element-type '(unsigned-byte 8)
                         :initial-element 255)
    ,(random-vector 1000)
    ,(random-vector 1000000)))

(defun %lzma-test (input)
  (apply #'lzma-decompress (multiple-value-list (lzma-compress input))))

(defun lzma-test (&optional quietp)
  (loop for input in *lzma-test-input*
        with output = nil
        do (let ((*print-length* 8))
             (unless quietp
               (format t "Testing vector ~S of length ~D..."
                       input (length input)))
             (setf output (%lzma-test input)))
        if (equalp input output)
          do (unless quietp (format t "Passed.~%"))
        else do (error "Test failed for vector of length ~S.~%Input:~A~%"
                       (length input) input))
  (lzma-fuzz 1000 1000 quietp t))

(defun lzma-fuzz (iterations vector-length &optional quietp random-length-p)
  (let ((*random-state* (make-random-state t)))
    (loop for i below iterations
          for input = (random-vector (if random-length-p
                                         (1+ (random vector-length))
                                         vector-length))
          with output = nil
          do (let ((*print-length* 8))
               (unless quietp
                 (format t "Testing array ~S of length ~D..."
                         input (length input)))
               (setf output (%lzma-test input)))
          if (equalp input output)
            do (unless quietp (format t "Passed.~%"))
          else do (error "Test failed for vector of length ~S.~%Input:~A~%"
                         (length input) input)))
  (values))
