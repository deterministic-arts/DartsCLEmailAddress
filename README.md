
DartsCLEmailAddress
===================

This library provides a fully RFC 5322 compliant parser for
email addresses. Also provided are a few tiny helper functions,
which allow the formatting of email addresses in ways, which
ensures, that they are RFC 5322 compliant.

Package DARTS.LIB.EMAIL-ADDRESS
-------------------------------

- Function: `parse-rfc5322-addr-spec` _string_ `&key` _start_ _end_ _allow-unicode_ &rarr; _local-part_ _domain_ _error_
- Function: `parse-rfc5322-mailbox` _string_ `&key` _start_ _end_ _allow-unicode_ &rarr; _local-part_ _domain_ _display-name_ _error_
- Function: `parse-rfc5322-mailbox-list` _string_ `&key` _start_ _end_ _allow-unicode_ &rarr; _list_ _error_ _position_
- Function: `escape-local-part` _string_ &key _start_ _end_ &rarr; _result_
- Function: `escape-display-name` _string_ &key _start_ _end_ &rarr; _result_
- Structure: `address`
- Function: `address` _object_ &rarr; _address_
- Function: `address-string` _object_ &rarr; _string_
- Function: `address-local-part` _object_ &rarr; _string_
- Function: `address-domain` _object_ &rarr; _string_
- Function: `address-hash` _object_ &rarr; _result_
- Class: `mailbox`
- Class: `basic-mailbox`
- Function: `mailbox` _object_ &rarr; _mailbox_
- Generic Function: `mailboxp` _object_ &rarr; _result_
- Generic Function: `mailbox-address` _object_ &rarr; _address_
- Generic Function: `mailbox-display-name` _object_ &rarr; _result_
- Generic Function: `mailbox-local-part` _object_ &rarr; _result_
- Generic Function: `mailbox-domain` _object_ &rarr; _result_
- Generic Function: `mailbox-string` _object_ &rarr; _result_
