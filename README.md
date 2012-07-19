
DartsCLEmailAddress
===================

This library provides a fully RFC 5322 compliant parser for
email addresses. Also provided are a few tiny helper functions,
which allow the formatting of email addresses in ways, which
ensures, that they are RFC 5322 compliant.

Package DARTS.LIB.EMAIL-ADDRESS
-------------------------------

- Function: `parse-rfc5322-addr-spec STRING &optional START END => LOCAL-PART DOMAIN ERROR POSITION`
- Function: `parse-rfc5322-mailbox STRING &optional START END => LOCAL-PART DOMAIN DISPLAY-NAME ERROR POSITION`
- Function: `parse-rfc5322-mailbox-list STRING &optional START END => LIST ERROR POSITION`
- Function: `escape-local-part STRING &optional START END => STRING`
- Function: `escape-domain STRING &optional START END => STRING`
- Function: `escape-display-name STRING &optional START END => STRING`

