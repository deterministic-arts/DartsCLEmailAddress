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

(in-package "DARTS.TEST.EMAIL-ADDRESS")

(in-root-suite)

(defsuite parser-test-suite)
(defsuite address-test-suite)
(defsuite mailbox-test-suite)



(in-suite parser-test-suite)

(deftest test-address-spec-parser ()
  (let ((data '(("l@d" "l" "d" nil)
                ("l@[d]" "l" "[d]" nil)
                ("\"1 2\"@d" "1 2" "d" nil)
                ("l" "l" nil :missing-separator)
                ("l@" "l" nil :bad-domain)
                ("!@!" "!" "!" nil)
                ("(c)!(c)@(c)!(c)" "!" "!" nil)
                ("(c(c))!(c(c))@(c(c))!(c(c))" "!" "!" nil)
                )))
    (loop
      :for (input expected-local expected-domain expected-error) :in data
      :do (multiple-value-bind (actual-local actual-domain actual-error) (parse-rfc5322-addr-spec input)
            (is (eq expected-error actual-error))
            (is (equal expected-local actual-local))
            (is (equal expected-domain actual-domain))))))

(deftest test-mailbox-parser ()  
  (let ((data '(("l@d" "l" "d" nil nil)
                ("l@[d]" "l" "[d]" nil nil)
                ("\"1 2\"@d" "1 2" "d" nil nil)
                ("!@!" "!" "!" nil nil)
                ("D <d@d>" "d" "d" "D" nil)
                ("D (c) D <d@d>" "d" "d" "D D" nil)
                ("(c) D (c) D (c) < (c) d (c) @ (c) d (c) > (c)" "d" "d" "D D" nil)
                )))
    (loop
      :for (input expected-local expected-domain expected-display expected-error) :in data
      :do (multiple-value-bind (actual-local actual-domain actual-display actual-error) (parse-rfc5322-mailbox input)
            (is (eq expected-error actual-error))
            (is (equal expected-local actual-local))
            (is (equal expected-domain actual-domain))
            (is (equal expected-display actual-display))))))

#(deftest test-unicode-address-spec-parser ()
  (let ((data '(("ü@ä" "ü" "ä" nil)
                ("ü@[ä]" "ü" "[ä]" nil)
                ("\"Äüö Öüä\"@ß" "Äüö Öüä" "ß" nil)
                )))
    (loop
      :for (input expected-local expected-domain expected-error) :in data
      :do (multiple-value-bind (actual-local actual-domain actual-error) (parse-rfc5322-addr-spec input :allow-unicode t)
            (is (eq expected-error actual-error))
            (is (equal expected-local actual-local))
            (is (equal expected-domain actual-domain))))))



(in-suite address-test-suite)

(deftest test-address-case-preservation ()
  (let ((address (address "Dieter.Krebs@Heaven.COM")))
    (is (string= (address-local-part address) "Dieter.Krebs"))
    (is (string= (address-domain address) "Heaven.COM"))))

(deftest test-address-comparisons ()
  (labels
      ((pair= (p1 p2) 
         (and (string= (car p1) (car p2))
              (string-equal (cdr p1) (cdr p2))))
       (pair< (p1 p2)
         (let ((a1 (cdr p1)) (a2 (cdr p2)))
           (cond
             ((string-lessp a1 a2) t)
             ((string-lessp a2 a1) nil)
             (t (string< (car p1) (car p2))))))
       (pair (a b) (cons a b))
       (pair->address (p) (make-address (car p) (cdr p)))
       (truth (x) (and x t)))           ; ensure, true is T (not something non-NIL)
    (let ((input (list (pair "a" "b") (pair "a" "B") (pair "A" "b") (pair "A" "B")
                       (pair "b" "c") (pair "b" "C") (pair "b" "b") (pair "b" "B")
                       (pair "B" "B"))))
      (loop
        :for op :in input
        :for oa := (pair->address op)
        :do (loop
              :for ip :in input
              :for ia := (pair->address ip)
              :do (macrolet ((ensure (operator definition)
                               `(is (eq (truth (,operator oa ia))
                                        (truth ,definition)))))
                    (ensure address= (pair= op ip))
                    (ensure address< (pair< op ip))
                    (ensure address<= (not (pair< ip op)))
                    (ensure address>= (not (pair< op ip)))
                    (ensure address> (pair< ip op))
                    (ensure address/= (not (pair= op ip)))))))))

(deftest test-address-hash-code ()
  (let ((input (list "lp@example.com" "lp@Example.COM" "Lp@example.com"
                     "Lp@Example.COM")))
    (loop
      :for os :in input
      :for oa := (address os)
      :do (loop
            :for is :in input
            :for ia := (address is)
            :do (when (address= oa ia)
                  (is (eql (address-hash oa) (address-hash ia)))
                  (is (typep (address-hash oa) '(and fixnum (integer 0)))))))))

(deftest test-address-stringification ()
  (let ((input (list "a@b" "A@b" "a@B" "A@B"
                     "\"a a\"@b" "\"a A\"@b" "\"A a\"@b" "\"A A\"@b")))
    (loop
      :for str :in input
      :for a1 := (address str)
      :for r1 := (address-string a1)
      :for a2 := (address r1)
      :do (is (string= (address-local-part a1) (address-local-part a2)))
          (is (string= (address-domain a1) (address-domain a2)))
          (is (address= a1 a2)))))
