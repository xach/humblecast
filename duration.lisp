;;;; duration.lisp

(in-package #:humblecast)

(defun ffprobe-file (file)
  (with-output-to-string (stream)
    (let ((process
           (sb-ext:run-program "ffprobe"
                               (list (sb-ext:native-namestring (truename file)))
                               :search t
                               :output stream
                               :error stream)))
      (let ((code (sb-ext:process-exit-code process)))
        (unless (eql code 0)
          (error "ffprobe exited with status ~D" code))))))

(defun mp3-file-duration (file)
  "Return the duration of FILE in seconds, or NIL if the duration
cannot be determined."
  (let ((output (ffprobe-file file)))
    (ppcre:register-groups-bind ((#'parse-integer hours minutes seconds))
        (" Duration: (..):(..):(..)" output)
      (+ (* hours 3600)
         (* minutes 60)
         seconds))))

(defun decode-duration (seconds)
  (multiple-value-bind (hours seconds)
      (truncate seconds 3600)
    (multiple-value-bind (minutes seconds)
        (truncate seconds 60)
      (values hours minutes seconds))))

(defun pretty-duration (seconds)
  (multiple-value-call #'format nil "~2,'0D:~2,'0D:~2,'0D"
                       (decode-duration seconds)))
