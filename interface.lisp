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

(defun no-debugger-hook (condition old-hook)
  (declare (ignore old-hook))
  (format *error-output*
          "; Exiting on condition of type ~A: ~A~%"
          (type-of condition)
          condition)
  (finish-output *error-output*)
  (sb-ext:exit :code 1 :abort t))

(defun no-debugger ()
  (setf sb-ext:*invoke-debugger-hook* 'no-debugger-hook))

(defun exit-usage (&optional (exit-code 0))
  (format t "Usage: humblecast STORE-DIRECTORY~%")
  (sb-ext:exit :code exit-code))

(defun main (argv)
  (no-debugger)
  (unless (second argv)
    (exit-usage 1))
  (when (equal (second argv) "--help")
    (exit-usage))
  (let ((store-pathname (second argv)))
    (unless (probe-file store-pathname)
      (error "Store pathname ~A does not exist" store-pathname))
    (initialize store-pathname)
    (let ((new-items (update)))
      (when new-items
        (format t "~D new item~:P:" (length new-items))
        (dolist (item new-items)
          (format t "  ~A~%" (title item)))))
    (sb-ext:exit :code 0)))

