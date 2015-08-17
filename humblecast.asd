;;;; humblecast.asd

(asdf:defsystem #:humblecast
  :description "Generate and host The humble Farmer podcast"
  :author "Zach Beane <xach@xach.com>"
  :license "MIT"
  :depends-on (#:drakma
               #:zs3
               #:cxml
               #:closer-mop
               #:clss)
  :serial t
  :components ((:file "package")
               (:file "specials")
               (:file "jpeg")
               (:file "utility")
               (:file "protocols")
               (:file "pubdate")
               (:file "duration")
               (:file "store")
               (:file "scraping")
               (:file "podcast")
               (:file "generate-xml")
               (:file "s3")
               (:file "humblecast")
               (:file "update")
               (:file "interface")))

