(require :sb-bsd-sockets)
(defpackage smtp-client
 (:use :cl :sb-bsd-sockets))

(in-package smtp-client)

;;-----------------------------------------------------------------------------
;;
;; Really Naive SMTP Client
;; - AbstractOwl <https://github.com/abstractOwl>
;;
;; Parts used from sbcl contrib/sb-bsd-sockets/tests.lisp
;; <https://gitorious.org/sbcl/sbcl/source/3b91bf9e9daf110b35dd8d5b6ca5a88e0fb0f13b:contrib/sb-bsd-sockets/tests.lisp#L75-122>
;;
;; Don't use this for anything serious; it does no error checking or anything.
;; Just foolishly continues sending requests, regardless of status code.
;;
;;-----------------------------------------------------------------------------
;;
;; Basic Requests
;; --------------
;;
;; EHLO example.com\n
;; MAIL FROM: John Doe <somebody@example.com>\n
;; RCPT TO: Jane Doe <nobody@example.com>\n
;; DATA.\n
;; Some message\n
;; .\n
;; QUIT
;;
;;-----------------------------------------------------------------------------

;; Vars

(defvar dest)
(defvar domain)
(defvar from)
(defvar to)
(defvar subject)
(defvar message)


;; Functions

(defun prompt (line)
 "Display message and return input"
 (format t "~10A" line)
 (finish-output)
 (read-line))

(defun read-buf-nonblock (buffer stream)
  "Like READ-SEQUENCE, but returns early if the full quantity of data isn't there to be read.  Blocks if no input at all"
  (let ((eof (gensym)))
    (do ((i 0 (1+ i))
         (c (read-char stream nil eof)
            (read-char-no-hang stream nil eof)))
        ((or (>= i (length buffer)) (not c) (eq c eof)) i)
      (setf (elt buffer i) c))))

(defun stream-send (out line)
 "Write to socket."
 (format t "~A~C~C" line #\return #\linefeed)
 (format out "~A~C~C" line #\return #\linefeed)
 (force-output))

(defun stream-read (in)
 "Read from socket."
 (let* ((buf (make-string 250))
        (data (subseq buf 0 (read-buf-nonblock buf in))))
 (format t "+ ~S~%" (string-right-trim '(#\return #\linefeed) data))))



;; Get Input

;; Destination
(setq dest (prompt "DestHost:"))
;; EHLO domain\r\n
(setq domain (prompt "Domain:"))
;; MAIL FROM: <somebody@example.com>\r\n
(setq from (prompt "From:"))
;; RCPT TO: <nobody@example.com>\r\n
(setq to (prompt "To:"))
;; DATA\r\n
;; Subject: subject\r\n
;; message\r\n
;; .\r\n
(setq subject (prompt "Subject:"))
(setq message (prompt "Message:"))


;; DEBUG print
;;(format t "~%~%--- To ~A:25 ---~%~%" dest)
;;(format t
;; "EHLO ~A~%MAIL FROM: <~A>~%RCPT TO: <~A>~%DATA~%Subject: ~A~%~A~%.~%QUIT~%"
;; domain from to subject message)


;; Start Socket

(let ((s (make-instance 'inet-socket :type :stream :protocol :tcp))
      (dest-host (car (host-ent-addresses (get-host-by-name dest))))
      (line))
 (socket-connect s dest-host 25)
 (format t "> Connecting to ~A:25.~%" dest-host)

 (let ((stream (socket-make-stream s :input t :output t :buffering :none)))
  (stream-read stream)

  (stream-send stream (format nil "EHLO ~A" domain))
  (stream-read stream)

  (stream-send stream (format nil "MAIL FROM: ~A" from))
  (stream-read stream)

  (stream-send stream (format nil "RCPT TO: ~A" to))
  (stream-read stream)

  (stream-send stream (format nil "DATA"))
  (stream-read stream)

  (stream-send stream (format nil "Subject: ~A" subject))
  (stream-send stream (format nil "~A" message))
  (stream-send stream (format nil "."))
  (stream-read stream)

  (stream-send stream (format nil "QUIT"))
  (stream-read stream)

  (socket-close s)
))
