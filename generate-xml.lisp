;;;; generate-xml.lisp

(in-package #:humblecast)

(defun element (name value)
  (labels ((content (value)
             (if (consp value)
                 (apply #'elements value)
                 (text value))))
    (if (consp name)
        (with-element* ((first name) (second name))
          (content value))
        (with-element name (content value)))))

(defun elements (&rest names-and-values)
  (loop for (name value) on names-and-values by #'cddr
        do (element name value)))

(defgeneric generate (object))

(defmethod generate :around ((podcast podcast))
  (with-xml-output (make-string-sink) (call-next-method)))

(defmethod generate ((podcast podcast))
  (with-element "rss"
    (attribute* "xmlns" "itunes"
                "http://www.itunes.com/dtds/podcast-1.0.dtd")
    (attribute "version" "2.0")
    (with-element "channel"
      (elements "title" (title podcast)
                "link" (link podcast)
                "description" (description podcast)
                "language" (language podcast)
                "copyright" (copyright podcast)
                "managingEditor" (managing-editor podcast)
                "pubDate" (pubdate-string (pub-date podcast))
                "lastBuildDate" (pubdate-string (last-build-date podcast))
                "generator" *generator-string*
                "docs" (docs podcast)
                '("itunes" "author") (author podcast)
                '("itunes" "explicit") (explicit podcast))
      (with-element* ("itunes" "image")
        (attribute "href" (image podcast)))
      (let ((categories (category podcast)))
        (when (atom categories)
          (setf categories (list categories)))
        (dolist (category categories)
          (with-element* ("itunes" "category")
            (attribute "text" category))))
      (elements  '("itunes" "owner") (list '("itunes" "name") (owner-name podcast)
                                           '("itunes" "email") (owner-email podcast))
                 '("itunes" "summary") (summary podcast)
                 '("itunes" "subtitle") (subtitle podcast))
      (with-element "image"
        (elements "url" (image podcast)
                  "title" (title podcast)
                  "link" (link podcast)))
      (when (completep podcast)
        (element '("itunes" "complete") "yes"))
      (when (new-feed-url podcast)
        (element '("itunes" "new-feed-url") (new-feed-url podcast)))
      (when (blocked podcast)
        (element '("itunes" "block") "yes"))
      (dolist (item (items podcast))
        (generate item)))))

(defmethod generate ((item item))
  (with-element "item"
    (elements "title" (title item)
              "link" (link item)
              "description" (description item)
              "pubDate" (pubdate-string (pub-date item))
              '("itunes" "explicit") (explicit item)
              '("itunes" "subtitle") (subtitle item)
              '("itunes" "summary") (summary item)
              '("itunes" "duration") (duration item))
    (with-element "guid"
      (attribute "isPermaLink" "false")
      (text (guid item)))
    (with-element "enclosure"
      (attribute "length" (size item))
      (attribute "type" "audio/mpeg")
      (attribute "url" (enclosure-url item)))))

(defun generate-to (object file)
  (with-open-file (stream file :direction :output
                          :if-exists :rename-and-delete)
    (write-string (generate object) stream))
  (probe-file file))
