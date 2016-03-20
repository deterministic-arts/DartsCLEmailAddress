#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Email Address Parser
  Copyright (c) 2011, 2016 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package "DARTS.LIB.EMAIL-ADDRESS")

#|
  quoted-pair     =   ("\" (VCHAR / WSP)) / obs-qp

  FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
                                          ; Folding white space

  ctext           =   %d33-39 /          ; Printable US-ASCII
                      %d42-91 /          ;  characters not including
                      %d93-126 /         ;  "(", ")", or "\"
                      obs-ctext

  ccontent        =   ctext / quoted-pair / comment

  comment         =   "(" *([FWS] ccontent) [FWS] ")"

  CFWS            =   (1*([FWS] comment) [FWS]) / FWS

  atext           =   ALPHA / DIGIT /    ; Printable US-ASCII
                       "!" / "#" /        ;  characters not including
                       "$" / "%" /        ;  specials.  Used for atoms.
                       "&" / "'" /
                       "*" / "+" /
                       "-" / "/" /
                       "=" / "?" /
                       "^" / "_" /
                       "`" / "{" /
                       "|" / "}" /
                       "~"

  atom            =   [CFWS] 1*atext [CFWS]

  dot-atom-text   =   1*atext *("." 1*atext)

  dot-atom        =   [CFWS] dot-atom-text [CFWS]

  specials        =   "(" / ")" /        ; Special characters that do
                      "<" / ">" /        ;  not appear in atext
                      "[" / "]" /
                      ":" / ";" /
                      "@" / "\" /
                      "," / "." /
                      DQUOTE

   qtext           =   %d33 /             ; Printable US-ASCII
                       %d35-91 /          ;  characters not including
                       %d93-126 /         ;  "\" or the quote character
                       obs-qtext

   qcontent        =   qtext / quoted-pair

   quoted-string   =   [CFWS]
                       DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                       [CFWS]

   word            =   atom / quoted-string

   phrase          =   1*word / obs-phrase

   mailbox         =   name-addr / addr-spec

   name-addr       =   [display-name] angle-addr

   angle-addr      =   [CFWS] "<" addr-spec ">" [CFWS] /
                       obs-angle-addr

   display-name    =   phrase

   addr-spec       =   local-part "@" domain

   local-part      =   dot-atom / quoted-string / obs-local-part

   domain          =   dot-atom / domain-literal / obs-domain

   domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]

   dtext           =   %d33-90 /          ; Printable US-ASCII
                       %d94-126 /         ;  characters not including
                       obs-dtext          ;  "[", "]", or "\"
|#

(defvar *allow-unicode* nil)
(defvar *allow-obsolete-syntax* nil)

(declaim (ftype (function (character) (values t))
                atext-char-p fws-char-p dtext-char-p ctext-char-p
                qtext-char-p))

(declaim (ftype (function (&optional fixnum) (vector character))
                make-buffer))


(defun atext-char-p (char)
  (or (char<= #\A char #\Z)
      (char<= #\a char #\z)
      (char<= #\0 char #\9)
      (position char "!#$%&'*+-/=?^_`{|}~")
      (and *allow-unicode* (> (char-code char) 127))))

(defun fws-char-p (char)
  (or (char= char #\tab)
      (char= char #\newline)
      (char= char #\return)
      (char= char #\space)))

(defun dtext-char-p (char)
  (or (char<= #.(code-char 33) char #.(code-char 90))
      (char<= #.(code-char 94) char #.(code-char 126))
      (and *allow-unicode* (> (char-code char) 127))))

(defun ctext-char-p (char)
  (or (char<= #.(code-char 33) char #.(code-char 39))
      (char<= #.(code-char 42) char #.(code-char 91))
      (char<= #.(code-char 93) char #.(code-char 126))
      (and *allow-unicode* (> (char-code char) 127))))

(defun qtext-char-p (char)
  (or (char= char #.(code-char 33))
      (char<= #.(code-char 35) char #.(code-char 91))
      (char<= #.(code-char 93) char #.(code-char 126))
      (and *allow-unicode* (> (char-code char) 127))))

(defmacro recurr (name bindings &body body)
  (let ((names (mapcar #'car bindings))
        (inits (mapcar #'cadr bindings)))
    `(labels ((,name (,@names) ,@body))
       (,name ,@inits))))


(defun make-buffer (&optional (length 64)) 
  (make-array (list length) 
    :element-type 'character 
    :adjustable t 
    :fill-pointer 0))


(defun next-token (string start &optional end)
  (let ((end (or end (length string))))
    (labels
        ((at (k) (char string k))
         (finish (buffer) (coerce buffer 'simple-string))
         (skip-comment (p depth)
           (if (>= p end)
               (values p nil)
               (let ((char (at p)))
                 (cond
                   ((char= char #\() (skip-comment (1+ p) (1+ depth)))
                   ((char= char #\)) (if (= depth 1) (skip-cfws (1+ p)) (skip-comment (1+ p) (- depth 1))))
                   ((ctext-char-p char) (skip-comment (1+ p) depth))
                   ((fws-char-p char) (skip-comment (1+ p) depth))
                   (t (values p nil))))))
         
         (skip-cfws (p)
           (if (>= p end)
               (values p t)
               (let ((char (at p)))
                 (cond
                   ((char= char #\() (skip-comment (1+ p) 1))
                   ((fws-char-p char) (skip-cfws (1+ p)))
                   (t (values p t))))))
         
         (parse-quoted-string (p buffer)
           (if (>= p end) 
               (values p :error nil)
               (let ((char (at p)))
                 (cond
                   ((char= char #\") (values (1+ p) :quoted-string (finish buffer)))
                   ((char= char #\\) 
                    (if (< (- end p) 2) 
                        (values p :error nil)
                        (progn (vector-push-extend (at (1+ p)) buffer)
                               (parse-quoted-string (+ p 2) buffer))))
                   ((fws-char-p char) 
                    (vector-push-extend char buffer)
                    (parse-quoted-string (1+ p) buffer))
                   ((qtext-char-p char)
                    (vector-push-extend char buffer)
                    (parse-quoted-string (1+ p) buffer))
                   (t (values p :error nil))))))

         (parse-domain-literal (p buffer)
           (if (>= p end) 
               (values p :error nil)
               (let ((char (at p)))
                 (cond
                   ((char= char #\]) 
                    (vector-push-extend char buffer)
                    (values (1+ p) :domain-literal (finish buffer)))
                   ((fws-char-p char) 
                    (vector-push-extend char buffer)
                    (parse-domain-literal (1+ p) buffer))
                   ((dtext-char-p char)
                    (vector-push-extend char buffer)
                    (parse-domain-literal (1+ p) buffer))
                   (t (values p :error nil))))))

         (parse-dot-atom (p)
           (let ((buffer (make-buffer)))
             (recurr iter ((p p) (state :start) (pure t))
               (if (>= p end)
                   (ecase state
                     ((:start) (values p :error nil))
                     ((:text) (values p (if pure :atom :dot-atom) (finish buffer)))
                     ((:dot) 
                      (cond
                        (*allow-obsolete-syntax* (vector-push-extend #\. buffer) (values p :phrase (finish buffer)))
                        (pure (- p 1) :atom (finish buffer))
                        (t (values (- p 1) :dot-atom (finish buffer))))))
                   (let ((char (at p)))
                     (cond
                       ((atext-char-p char)
                        (when (eq state :dot) (vector-push-extend #\. buffer))
                        (vector-push-extend char buffer)
                        (iter (1+ p) :text (and pure (not (eq state :dot)))))
                       ((char= #\. char) (iter (1+ p) :dot pure))
                       (t (ecase state
                            ((:start) (values p :error nil))
                            ((:text) (values p (if pure :atom :dot-atom) (finish buffer)))
                            ((:dot) 
                             (cond
                               (*allow-obsolete-syntax* (vector-push-extend #\. buffer) (values p :phrase (finish buffer)))
                               (pure (- p 1) :atom (finish buffer))
                               (t (values (- p 1) :dot-atom (finish buffer))))))))))))))

      (declare (inline at))
      (multiple-value-bind (position ok) (skip-cfws start)
        (cond 
          ((not ok) (values position :error nil))
          ((>= position end) (values position nil nil))
          (t (let ((char (at position)))
               (cond
                 ((char= char #\<) (values (1+ position) :< nil))
                 ((char= char #\@) (values (1+ position) :@ nil))
                 ((char= char #\>) (values (1+ position) :> nil))
                 ((char= char #\") (parse-quoted-string (1+ position) (make-buffer)))
                 ((char= char #\[) (let ((buffer (make-buffer))) (vector-push-extend #\[ buffer) (parse-domain-literal (1+ position) buffer)))
                 ((char= char #\,) (values (1+ position) :comma nil))
                 ((atext-char-p char) (parse-dot-atom position))
                 (t (values position :error nil))))))))))
         


(defun parse-rfc5322-addr-spec (string 
                                &key (start 0) end
                                     ((:allow-unicode *allow-unicode*) *allow-unicode*)
                                     (allow-trailing-junk nil))
  "parse-rfc5322-addr-spec STRING &key START END => LOCAL-PART DOMAIN POSITION

Parses an email address (rule addr-spec in RFC 5322) from STRING, starting at
index START (inclusive, defaults to 0), and stopping at index END (exclusive,
defaults to the length of STRING).

Returns four values

  1. the local part of the email address
  2. the domain part of the email address
  3. an error code, or nil, if the parsing was successful
  4. the index of the first character not processed

This function stops after finding a complete email address, when it encounters
an error, or when reaching the END index. The following error codes are defined:

  nil                no error, a full email address has been found
  :bad-local-part    no valid local part could be found
  :missing-separator the `@´ was not found
  :bad-domain        no valid domain part could be found
  :trailing-garbage  unprocessed characters remain after the address"

  (let ((local-part nil))
    (labels 
        ((jump (position parser)
           (multiple-value-bind (after token value) (next-token string position end)
             (funcall parser position after token value)))
         (parse-local-part (before after token value)
           (case token
             ((:atom :dot-atom :quoted-string) (setf local-part value) (jump after #'parse-at))
             (t (values nil nil :bad-local-part before))))
         (parse-at (before after token value)
           (declare (ignore value))
           (case token
             ((:@) (jump after #'parse-domain))
             (t (values local-part nil :missing-separator before))))
         (parse-domain (before after token value)
           (case token
             ((:dot-atom :atom :domain-literal) (finish local-part value after))
             (t (values local-part nil :bad-domain before))))
         (finish (local-part domain last-good)
           (if allow-trailing-junk
               (values local-part domain nil last-good)
               (multiple-value-bind (after token value) (next-token string last-good end)
                 (declare (ignore value))
                 (if (null token)
                     (values local-part domain nil after)
                     (values local-part domain :trailing-garbage last-good))))))
      (jump start #'parse-local-part))))


(defun parse-rfc5322-mailbox (string 
                              &key (start 0) end 
                                   ((:allow-unicode *allow-unicode*) *allow-unicode*)
                                   ((:allow-obsolete-syntax *allow-obsolete-syntax*) *allow-obsolete-syntax*)
                                   (allow-trailing-junk nil))
  "parse-rfc5322-mailbox STRING &optional START END => LOCAL-PART DOMAIN DISPLAY-NAME ERROR POSITION"
  (let ((end (or end (length string)))
        (local-part nil)
        (domain nil)
        (display-name nil)
        (phrase nil))
    (labels
        ((finish-buffer (buffer) (coerce buffer 'simple-string))
         (done (code position) 
           (if (or code allow-trailing-junk)
               (values local-part domain display-name code position)
               (multiple-value-bind (position* token) (next-token string position end)
                 (if (null token) 
                     (values local-part domain display-name code position*)
                     (values local-part domain display-name :trailing-garbage position)))))
         (copy-chars (src dst)
           (loop
              :for char :across src 
              :do (vector-push-extend char dst))
           dst)
         (finish-phrase () 
           (if (null phrase) nil
               (let ((phrase (nreverse phrase))
                     (buffer (make-buffer)))
                 (copy-chars (car phrase) buffer)
                 (loop
                    :for part :in (cdr phrase)
                    :do (progn
                          (vector-push-extend #\space buffer)
                          (copy-chars part buffer)))
                 (finish-buffer buffer))))
         (jump (position parser)
           (multiple-value-bind (after token value) (next-token string position end)
             (funcall parser position after token value)))
         (parse-start (before after token value)
           (case token
             ((:<) (jump after #'parse-enclosed))
             ((:phrase) (parse-display-name before after token value))
             ((:dot-atom :atom :quoted-string)
              (multiple-value-bind (after* token* value*) (next-token string after end)
                (case token*
                  ((:@) (setf local-part value) (jump after* #'parse-domain))
                  (otherwise (push value phrase) (parse-display-name before after* token* value*)))))
             ((nil) (done :missing-display-name before))
             (t (done :bad-display-name before))))
         (parse-domain (before after token value)
           (case token
             ((:domain-literal :atom :dot-atom) (setf domain value) (done nil after))
             ((nil) (done :missing-domain before))
             (t (done :bad-domain before))))
         (parse-enclosed (before after token value)
           (case token
             ((:dot-atom :atom :quoted-string) (setf local-part value) (jump after #'parse-enclosed-@))
             ((nil) (done :missing-local-part before))
             (t (done :bad-local-part before))))
         (parse-enclosed-@ (before after token value)
           (declare (ignore value))
           (case token
             ((:@) (jump after #'parse-enclosed-domain))
             (t (done :missing-separator before))))
         (parse-enclosed-domain (before after token value)
           (case token
             ((:atom :dot-atom :domain-literal) (setf domain value) (jump after #'parse-closing->))
             ((nil) (done :missing-domain before))
             (t (done :bad-domain before))))
         (parse-closing-> (before after token value)
           (declare (ignore value))
           (case token
             ((:>) (done nil after))
             (t (done :missing-delimiter before))))
         (parse-display-name (before after token value)
           (case token
             ((:atom :quoted-string :phrase) (push value phrase) (jump after #'parse-display-name))
             ((:<) (setf display-name (finish-phrase)) (jump after #'parse-enclosed))
             ((nil) (done :missing-address before))
             (t (done :bad-display-name before)))))
      (jump start #'parse-start))))


(defun parse-rfc5322-mailbox-list (string 
                                   &key (start 0) end 
                                        ((:allow-unicode *allow-unicode*) *allow-unicode*)
                                        ((:allow-obsolete-syntax *allow-obsolete-syntax*) *allow-obsolete-syntax*))
  (let ((end (or end (length string)))
        (answer ()))
    (labels
        ((make-mbox (local-part domain display-name)
           (list local-part domain display-name))
         (parse-mailbox (position)
           (multiple-value-bind (local-part domain display-name error after) 
               (parse-rfc5322-mailbox string :start position :end end :allow-trailing-junk t)
             (if error 
                 (values (nreverse answer) error position)
                 (progn (push (make-mbox local-part domain display-name) answer)
                        (parse-next-mailbox after)))))
         (parse-next-mailbox (position)
           (multiple-value-bind (after token value) (next-token string position end)
             (declare (ignore value))
             (case token
               ((:comma) (parse-mailbox after))
               ((nil) (values (nreverse answer) nil after))
               (t (values (nreverse answer) :comma-expected position))))))
      (multiple-value-bind (local-part domain display-name error after) 
          (parse-rfc5322-mailbox string :start start :end end :allow-trailing-junk t)
        (case error
          ((nil) (push (make-mbox local-part domain display-name) answer) (parse-next-mailbox after))
          ((:missing-display-name) (values nil nil after))
          (t (values nil error start)))))))


(defun escape-local-part (string &key (start 0) end)
  "escape-local-part STRING &optional START END => ANSWER

Returns a copy of the portion of STRING between START (incl.) and
END (excl.). If necessary, this function adds quotes around the result,
and makes sure, that internal occurrences of #\\\\ and #\\\" are 
properly escaped."
  (let ((end (or end (length string))))
    (flet ((escape-it (prefix-length)
             (let ((buffer (make-buffer)))
               (vector-push-extend #\" buffer)
               (loop
                  :for k :upfrom start :below prefix-length
                  :do (vector-push-extend (char string k) buffer))
               (loop
                  :for k :upfrom prefix-length :below end
                  :for char := (char string k)
                  :do (cond
                        ((char= char #\\) (vector-push-extend #\\ buffer) (vector-push-extend #\\ buffer))
                        ((char= char #\") (vector-push-extend #\\ buffer) (vector-push-extend #\" buffer))
                        ((fws-char-p char) (vector-push-extend char buffer))
                        ((> (char-code char) 127) (vector-push-extend char buffer))
                        ((qtext-char-p char) (vector-push-extend char buffer))
                        (t (vector-push-extend #\\ buffer)
                           (vector-push-extend char buffer))))
               (vector-push-extend #\" buffer)
               (coerce buffer 'simple-string))))
      (loop
         :with state := :start
         :for position :upfrom start :below end
         :for char := (char string position)
         :do (cond
               ((atext-char-p char) (setf state :text))
               ((> (char-code char) 127) (setf state :text))
               ((char= char #\.)
                (case state
                  ((:text) (setf state :dot))
                  (t (return-from escape-local-part (escape-it position)))))
               (t (return-from escape-local-part (escape-it position))))
         :finally (case state
                    ((:text) (return (subseq string start end)))
                    (t (return-from escape-local-part (escape-it position))))))))


(defun escape-display-name (string &key (start 0) end)
  "escape-display-name STRING &optional START END => ANSWER

Returns a copy of the portion of STRING between START (incl.) and
END (excl.). If necessary, this function adds quotes around the result,
and makes sure, that it satisfies the rules of an email display name."
  (let ((end (or end (length string))))
    (flet ((escape-it (prefix-length)
             (let ((buffer (make-buffer)))
               (vector-push-extend #\" buffer)
               (loop
                  :for k :upfrom start :below prefix-length
                  :do (vector-push-extend (char string k) buffer))
               (loop
                  :for k :upfrom prefix-length :below end
                  :for char := (char string k)
                  :do (cond
                        ((char= char #\\) (vector-push-extend #\\ buffer) (vector-push-extend #\\ buffer))
                        ((char= char #\") (vector-push-extend #\\ buffer) (vector-push-extend #\" buffer))
                        ((fws-char-p char) (vector-push-extend char buffer))
                        ((> (char-code char) 127) (vector-push-extend char buffer))
                        ((qtext-char-p char) (vector-push-extend char buffer))
                        (t (vector-push-extend #\\ buffer)
                           (vector-push-extend char buffer))))
               (vector-push-extend #\" buffer)
               (coerce buffer 'simple-string))))
      (loop
         :for position :upfrom start :below end
         :for char := (char string position)
         :do (unless (or (atext-char-p char) (> (char-code char) 127))
               (return-from escape-display-name (escape-it position)))
         :finally (return (subseq string start end))))))
