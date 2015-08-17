;;;; mp3info.lisp

(in-package #:humblecast)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&key size)
  `(simple-array octet (,size)))

(defmacro with-octet-input ((stream file) &body body)
  `(with-open-file (,stream ,file :direction :input :element-type 'octet)
     ,@body))

(defun make-octet-vector (size)
  (make-array size :element-type 'octet))

(defun first-n-bytes (file n &key (at 0))
  (let ((buffer (make-octet-vector n)))
    (with-open-file (stream file :element-type 'octet)
      (file-position stream at)
      (let ((last-index (read-sequence buffer stream)))
        (subseq buffer 0 last-index)))))

(defun to-bits (buffer)
  (map nil (lambda (octet)
             (format t "~8,'0B " octet))
       buffer))

(defmacro bit32-bind ((&rest bindings) integer &body body)
  (let ((ginteger (gensym)))
    `(let ((,ginteger ,integer))
       (let ,(loop for (variable bit-length) in bindings
                   for offset = (- 32 bit-length) then (- offset bit-length)
                   when variable
                   collect `(,variable (ldb (byte ,bit-length ,offset) ,ginteger)))
         ,@body))))

(defun frame-length (frame-header)
  (bit32-bind ((syncword 12)
               (algorithm 1)
               (layer 2)
               (protection-bit 1)
               (bitrate-index 4)
               (sampling-frequency 2)
               (padding-bit 1)
               (private-bit 1)
               (mode 2)
               (mode-extension 2)
               (copyright 1)
               (orignal/home 1)
               (emphasis 2))
      frame-header
    (when (and (= sync-bits #b11111111111)
               (= version-id #b10)
               (= layer-spec #b01)
               (/= sample-rate-code #b11)
               (/= bit-rate-code #b1111))
      (let* ((length 0)
             (sample-rates #(22050 24000 16000))
             (bit-rates #(0 8 16 24 32 40 48 56 64 80 96 112 128 144 160))
             (bit-rate (* 1000 (aref bit-rates bit-rate-code)))
             (sample-rate (aref sample-rates sample-rate-code)))
        (when (zerop crc-indicator-bit)
          (incf length 2))
        (values (+ length padding-indicator-bit
                   (floor (* 144 bit-rate) sample-rate))
                bit-rate
                sample-rate
                bit-rate-code)))))

(defun frame-check (frame-header)
  (bit32-bind ((syncword 12)
               (algorithm 1)
               (layer 2)
               (protection-bit 1)
               (bitrate-index 4)
               (sampling-frequency 2)
               (padding-bit 1)
               (private-bit 1)
               (mode 2)
               (mode-extension 2)
               (copyright 1)
               (orignal/home 1)
               (emphasis 2))
      frame-header
    (and (= syncword #xFFF)
         (= layer #b01)
         (/= bitrate-index #b1111)
         (/= sampling-frequency #b11)
         (list :syncword syncword
               :algorithm algorithm
               :layer layer
               :protection-bit protection-bit
               :bitrate-index bitrate-index
               :sampling-frequency sampling-frequency
               :padding-bit padding-bit
               :private-bit private-bit
               :mode mode
               :mode-extension mode-extension
               :copyright copyright
               :original/home orignal/home
               :emphasis emphasis))))

(defun read-frame-header (stream)
  (let ((buffer (make-octet-vector 4)))
    (let ((last-index (read-sequence buffer stream)))
      (unless (= 4 last-index)
        (error "Unexpected short read"))
      (+ (ash (aref buffer 0) 24)
         (ash (aref buffer 1) 16)
         (ash (aref buffer 2)  8)
         (ash (aref buffer 3)  0)))))

(defun skip-n-octets (n stream)
  (let ((position (file-position stream)))
    (file-position stream (+ position n))))

(defun reset-stream (stream)
  (file-position stream :start))

(defun find-frame-header (stream)
  (loop
    (let ((start-position (file-position stream))
          (value (read-byte stream nil)))
      (when (null value)
        (return))
      (when (= value 255)
        (file-position stream start-position)
        (let ((header (read-frame-header stream)))
          (bit32-bind ((sync-bits 11))
              header
            (file-position stream start-position)
            (if (and (= sync-bits #b11111111111)
                     (frame-length header))
                (return header)
                (read-byte stream))))))))

(defun dump-file-frames (file &key (at 0))
  (with-open-file (stream file :element-type 'octet)
    (skip-n-octets at stream)
    (let ((frame-number 0)
          (samples 1152)
          (total-length 0))
      (loop
        (let ((header (find-frame-header stream)))
          (unless header
            (return total-length))
          (multiple-value-bind (length bit-rate sample-rate)
              (frame-length header)
            (when (plusp bit-rate)
              (incf total-length (/ length (* bit-rate 125))))
            (format t "~&; frame ~D: ~D ~D ~D~%"
                    (incf frame-number)
                    length
                    bit-rate sample-rate)
            (skip-n-octets 1 stream)))))))

(defun count-file-frames (file)
  (with-octet-input (stream file)
    (let ((b1 (read-byte stream))
          (b2 (read-byte stream))
          (b3 (read-byte stream))
          (b4 (read-byte stream))
          (count 0))
      (let ((header (logior (ash b1 24)
                            (ash b2 16)
                            (ash b3 8)
                            (ash b4 0))))
        (loop
          (format t "~8,'0X" header)
          (when (frame-check header)
            (incf count)
            (format t " ** at ~A"
                    (- (file-position stream) 4)))
          (terpri)
          (let ((b (read-byte stream nil)))
            (unless b
              (return count))
            (setf header (logand #xFFFFFFFF
                                 (logior (ash header 8) b)))))))))
