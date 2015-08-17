;;;; utility.lisp

(in-package #:humblecast)

(defun bhex (array)
  (ironclad:byte-array-to-hex-string array))

(defun digest (string)
  (bhex
   (ironclad:digest-sequence :sha1
                             (babel:string-to-octets string :encoding :utf-8))))

(defun file-digest (pathname)
  (bhex (ironclad:digest-file :sha1 pathname)))
