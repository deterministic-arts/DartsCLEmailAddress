#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Email Address Parser
  Copyright (c) 2016 Dirk Esser

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

(defclass mailbox () ()
  (:documentation "A mailbox is basically an email address combined with an
    optional display name for that address. This class itself is only a protocol
    class, and exists for the purpose of type discrimination only."))

(defgeneric mailboxp (object)
  (:documentation "Tests, whether `object' fulfills the `mailbox' protocol.
    This condition is always true by definition for subclasses of class `mailbox'.
    It may additionally be true for other objects."))

(defgeneric mailbox-address (object)
  (:documentation "Answers the `address' instance, which describes the actual
    email address associated with mailbox `object'. This method is part of the
    core mailbox protocol, and must be implemented by all objects, which want
    to participate in that protocol."))

(defgeneric mailbox-display-name (object)
  (:documentation "Answers the display name associated with the given mailbox
    instance `object'. This function is part of the core mailbox protocol and
    must be implemented by all objects, which want to participate in that 
    protocol."))

(defgeneric mailbox-string (object)
  (:documentation "Constructs a string representation of the given mailbox
    instance. The result is required to be a well-formed RFC 5322 email address
    parsable using the `mailbox' production. The default method should be usable
    by almost all concrete `mailbox' implementations."))

(defgeneric mailbox-local-part (object)
  (:documentation "Answers the local part string of this mailbox's address.
    The default method simply extracts the `address-local-part' from the object
    returned by `mailbox-address' when applied to the given `object'."))

(defgeneric mailbox-domain (object)
  (:documentation "Answers the domain string of this mailbox's address.
    The default method simply extracts the `address-domain' from the object
    returned by `mailbox-address' when applied to the given `object'."))



(defmethod mailbox-local-part (object)
  (address-local-part (mailbox-address object)))

(defmethod mailbox-domain (object)
  (address-domain (mailbox-address object)))

(defmethod mailbox-string (object)
  (let ((name (mailbox-display-name object))
        (addr (mailbox-address object)))
    (if (not name)
        (mailbox-string (mailbox-address object))
        (concatenate 'string (escape-display-name name)
                     " <" (address-string addr) ">"))))



(defstruct (address (:copier nil)
                    (:predicate addressp)
                    (:constructor %make-addr (local-part domain)))
  "Plain email address, composed of the local part (a string) and the
   domain (another string). Instances of this structure class are immutable
   after construction. This structure class fully supports the mailbox
   protocol."
  (local-part (error "missing local part") :type simple-string :read-only t)
  (domain (error "missing domain") :type simple-string :read-only t)
  (%string nil :type (or null simple-string))
  (%hash -1 :type fixnum))


(defun make-address (local-part domain)
  "Creates a new address from the given component values `local-part' and
   `domain'. Both arguments must be strings. Note, that this function currently
   does not validate the contents of the given local part and domain values."
  (%make-addr (coerce (copy-seq local-part) 'simple-string)
              (coerce (copy-seq domain) 'simple-string)))


(defun address-hash (object)
  (let ((hash (address-%hash object)))
    (if (not (minusp hash)) hash
        (let* ((h1 (sxhash (address-local-part object)))
               (h2 (sxhash (string-downcase (address-domain object)))))
          (setf (address-%hash object) (logxor h1 h2))))))


(defun address-string (object)
  (or (address-%string object)
      (setf (address-%string object)
            (let* ((loc (escape-local-part (address-local-part object)))
                   (dom (address-domain object)))
              (coerce (concatenate 'string loc "@" dom) 'simple-string)))))


(defmethod print-object ((object address) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (address-string object))))


(defun address (value)
  (labels
      ((fail ()
         (error 'simple-type-error
                :datum value :expected-type 'address
                :format-control "~S is not a email address designator"
                :format-arguments (list value)))
       (from-object (object)
         (typecase object
           (address object)
           (mailbox (from-object (mailbox-address object)))
           (t (fail))))
       (from-string (string)
         (multiple-value-bind (local domain display error) (parse-rfc5322-mailbox string)
           (declare (ignore display))
           (if (not error)
               (make-address local domain)
               (fail)))))
    (typecase value
      (address value)
      (mailbox (from-object (mailbox-address value)))
      (string (from-string value))
      (symbol (from-string (symbol-name value)))
      (t (fail)))))
       
(defun address= (a1 a2)
  (and (string= (address-local-part a1) (address-local-part a2))
       (string-equal (address-domain a1) (address-domain a2))))

(defun address/= (a1 a2)
  (or (string/= (address-local-part a1) (address-local-part a2))
      (string-not-equal (address-domain a1) (address-domain a2))))

(defun address< (a1 a2)
  (let ((d1 (address-domain a1))
        (d2 (address-domain a2)))
    (cond
      ((string< d1 d2) t)
      ((string< d2 d1) nil)
      (t (string-lessp (address-domain a1)
                       (address-domain a2))))))

(defun address<= (a1 a2)
  (not (address< a2 a1)))

(defun address> (a1 a2)
  (address< a2 a1))

(defun address>= (a1 a2)
  (address<= a2 a1))


(defmethod mailbox-address ((object address))
  object)

(defmethod mailbox-display-name ((object address))
  nil)

(defmethod mailbox-string ((object address))
  (concatenate 'string "<" (address-string object) ">"))

(defmethod mailbox-local-part ((object address))
  (address-local-part object))

(defmethod mailbox-domain ((object address))
  (address-domain object))



(defmethod mailboxp (object) (declare (ignore object)) nil)
(defmethod mailboxp ((object mailbox)) (declare (ignore object)) t)
(defmethod mailboxp ((object address)) (declare (ignore object)) t)

(defclass basic-mailbox (mailbox)
  ((mailbox-address
     :type address
     :reader mailbox-address)
   (mailbox-display-name
     :type (or null string) :initform nil :initarg :display-name
     :reader mailbox-display-name))
  (:documentation "A simple concrete implementation of `mailbox', which
    stores the address and display name information in dedicated slots."))

(defmethod shared-initialize :after ((object basic-mailbox) slots &key (address nil have-address))
  (declare (ignore slots))
  (when have-address
    (setf (slot-value object 'mailbox-address) (address address))))

(defmethod print-object ((object mailbox) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[~S ~]~S"
            (mailbox-display-name object)
            (address-string (mailbox-address object)))))

(defun mailbox (value)
  "Coerces the given `value' into an instance of class `mailbox' or
   a suitable subclass.

     - if `value' is already a `mailbox', it is directly returned

     - if `value' is an `address', a new mailbox instance is created,
       using that address and a display name value of nil.

     - if `value' is a string, it is parsed according to the RFC 5322
       `mailbox' production and a mailbox instance is created from the
       results.

    If the value cannot be coerced, signals a condition of type 
    `type-error'."
  (labels
      ((fail ()
         (error 'simple-type-error
                :datum value :expected-type 'mailbox
                :format-control "~S is not a mailbox designator"
                :format-arguments (list value)))
       (from-string (string)
         (multiple-value-bind (local domain display error) (parse-rfc5322-mailbox string)
           (if (not error)
               (make-instance 'basic-mailbox :address (make-address local domain) :display-name display)
               (fail)))))
    (typecase value
      (address (make-instance 'basic-mailbox :address value))
      (mailbox value)
      (string (from-string value))
      (symbol (from-string (symbol-name value)))
      (t (fail)))))
