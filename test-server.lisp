;;; Copyright (c) 2022 Carnegie Mellon University
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies
;;; or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(ql:quickload '(:alexandria :iterate :usocket-server :babel))

(defpackage :test-cava
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames (:us :usocket)
                    (:bb :babel))
  (:export #:run))

(in-package :test-cava)

(defun run (&optional (file "test-server-data.json"))
  (with-open-file (stream file)
    (iter (with sock := (us:socket-connect "127.0.0.1" 9017 :protocol :datagram))
          (for line := (read-line stream nil))
          (while line)
          (for buff := (bb:string-to-octets line))
          (us:socket-send sock buff (length buff))
          (for resp := (bb:octets-to-string (us:socket-receive sock nil 1000)
                                            :encoding :utf-8))
          (format t "~A~%" (subseq resp 0 (position #\Nul resp))))))
