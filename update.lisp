;;;; update.lisp

(in-package #:humblecast)

(defun podcast-bucket ()
  (or (attribute-value (list "s3" "bucket") *store*)
      (error "No key for (\"s3\" \"bucket\") in store")))

(defun last-update-time ()
  (or (sget "last-update" *store*)
      0))

(defun (setf last-update-time) (new-value)
  (setf (sget "last-update" *store*) new-value))

(defun mp3-links (page)
  (let* ((node (plump:parse (alexandria:read-file-into-string (file page)
                                                              :external-format :latin-1)))
         (anchors (clss:select "a[href$=mp3]" node)))
    (map 'list
         (lambda (anchor)
           (make-instance 'mp3-link
                          :url (merge-urls (plump:attribute anchor "href")
                                           (base page))
                          :description
                          (trim-anchor-text (plump:text anchor))))
         anchors)))

(defun index-page-links ()
  (mp3-links (fetch *source-url*)))

(defun cached-links ()
  (stored-objects-of-type 'mp3-link))

(defun new-links ()
  (let* ((cached (cached-links))
         (indexed (index-page-links)))
    (set-difference indexed cached
                               :key #'url
                               :test #'string=)))

(defun podcast-file ()
  (merge-store-pathname *store* "podcast/podcast.rss"))

(defun cached-items ()
  (sort (stored-objects-of-type 'humble-item)
        #'>
        :key #'pub-date))

(defun generate-new-podcast-feed ()
  (let ((feed (make-instance 'humblecast
                             :items (cached-items)))
        (feed-file (podcast-file)))
    (ensure-directories-exist feed-file)
    (alexandria:write-string-into-file (generate feed) feed-file
                                       :if-exists :supersede)
    (probe-file feed-file)))

(defun publish-podcast-feed ()
  (let ((file (podcast-file))
        (bucket (podcast-bucket))
        (key "podcast.rss"))
    (unless (probe-file file)
      (error "No podcast file found at ~S" file))
    (zs3:put-file file bucket key
                  :public t
                  :content-type "application/rss+xml")
    (zs3:resource-url :bucket bucket
                      :key key
                      :vhost :cname)))

(defun update (&optional (*store* *store*) &aux new-links)
  (when (setf new-links (new-links))
    (dolist (new-link new-links)
      (process-link new-link))
    (generate-new-podcast-feed)
    (publish-podcast-feed)))
