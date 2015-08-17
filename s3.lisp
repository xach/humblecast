;;;; s3.lisp

(in-package #:humblecast)

(defun url-file (url)
  (file-namestring (puri:uri-path (puri:uri url))))

(defun publish-cover-image (file)
  (multiple-value-bind (width height)
      (jpeg-dimensions file)
    (unless (and width height)
      (error "No dimensions found in ~A -- not a JPEG?" file))
    (unless (and (<= 1400 height)
                 (<= 1400 width))
      (error "Image should be >1400px square"))
    (zs3:put-file file (podcast-bucket) "cover.jpg"
                  :public t
                  :content-type "image/jpeg")
    (values (zs3:resource-url :bucket (podcast-bucket)
                              :key "cover.jpg"
                              :vhost :cname)
            width height)))
