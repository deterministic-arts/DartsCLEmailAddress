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

(defpackage "DARTS.LIB.EMAIL-ADDRESS"
  (:use "COMMON-LISP")
  (:export "*ALLOW-UNICODE*" "PARSE-RFC5322-ADDR-SPEC" 
           "PARSE-RFC5322-MAILBOX" "PARSE-RFC5322-MAILBOX-LIST"
           "ESCAPE-LOCAL-PART" "ESCAPE-DISPLAY-NAME" "ADDRESS" "ADDRESSP" 
           "ADDRESS-LOCAL-PART" "ADDRESS-DOMAIN" "ADDRESS-STRING" "ADDRESS-HASH" 
           "ADDRESS=" "ADDRESS/=" "ADDRESS<" "ADDRESS<=" "ADDRESS>=" "ADDRESS>"
           "MAILBOX" "BASIC-MAILBOX" "MAILBOXP" "MAILBOX-STRING" "MAILBOX-DISPLAY-NAME"
           "MAILBOX-ADDRESS" "MAILBOX-LOCAL-PART" "MAILBOX-DOMAIN" "MAKE-ADDRESS"))

