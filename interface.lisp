;;;; interface.lisp

(in-package #:humblecast)

(defun check-store ()
  (unless (lookup (list "s3" "credentials"))
    (error "No credentials in *STORE*")))

(defun save-credentials ()
  (setf (sget (list "s3" "credentials") *store*)
        (list (zs3:access-key zs3:*credentials*)
              (zs3:secret-key zs3:*credentials*))))

(defun initialize (store-pathname)
  (setf *store* (open-store store-pathname))
  (check-store)
  (setf zs3:*credentials* (lookup (list "s3" "credentials")))
  *store*)


