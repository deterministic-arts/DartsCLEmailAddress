
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

  Instances of this structure represent email addresses. Basically, an address
  is a pair of "local part" and "domain". After construction, `address` instances
  are immutable.

  This library defines a total ordering over all addresses, which is derived
  from the lexicographic orderings of the components. When comparing for order
  (i.e., using `address<`, `address<=`, `address>=` or `address>`) the domain
  part is always compared first. If ambigous (i.e., if both address instances
  have equal domains), the local parts are compared. 

  Regardless of whether the comparison is for order or for equality, the
  domain parts are always compared disregarding the letter case, and the local
  parts are always compared case-sensitively.

- Function: `address` _object_ &rarr; _address_

  Tries to coerce its argument _object_ into an instance of structure class
  `address`, according to the following rules:

    - if _object_ is already an instance of `address`, it is returned directly

    - if _object_ is a string, it is parsed according to the RFC `mailbox`
      production, and the results are used to construct a new `address`. If a
      display name part is present in _object_, it will be ignored.

  If this function cannot convert its argument into an `address`, it signals
  an error of type `type-error`.


- Function: `address-local-part` _object_ &rarr; _string_

  Answers the string, which is the local part of email address _object_

- Function: `address-domain` _object_ &rarr; _string_

  Answers the string, which is the domain part of email address _object_

- Function: `address-string` _object_ &rarr; _string_

  Answers the fully escaped string representation of email address  _object_.
  The _string_ returned by this function may be parsed back into an `address` 
  instance (e.g. by calling the `address` function), and the resulting
  `address` instance should be equivalent with _object_ under `address=`.

- Function: `address-hash` _object_ &rarr; _result_

  Answers a hash code for address instance _object_

- Function: `address=` _address1_ _address2_ &rarr; _result_

  Compares the addresses _address1_ and _address2_, and answers true, if
  both represent the same email address, and false otherwise.

- Function: `address/=` _address1_ _address2_ &rarr; _result_

  Compares the addresses _address1_ and _address2_, and answers true, if
  both represent different email addresses, and false otherwise.

- Function: `address<` _address1_ _address2_ &rarr; _result_

  Compares the addresses _address1_ and _address2_, and answers true, if
  _address1_ is strictly less than _address2_. See the description of
  structure class `address` for details about address ordering.

- Function: `address<=` _address1_ _address2_ &rarr; _result_

  Compares the addresses _address1_ and _address2_, and answers true, if
  _address1_ is less than or equal to _address2_. See the description of
  structure class `address` for details about address ordering.

- Function: `address>=` _address1_ _address2_ &rarr; _result_

  Compares the addresses _address1_ and _address2_, and answers true, if
  _address1_ is greater than or equal to _address2_. See the description of
  structure class `address` for details about address ordering.

- Function: `address>` _address1_ _address2_ &rarr; _result_

  Compares the addresses _address1_ and _address2_, and answers true, if
  _address1_ is strictly greater than _address2_. See the description of
  structure class `address` for details about address ordering.

- Class: `mailbox`
- Class: `basic-mailbox`
- Function: `mailbox` _object_ &rarr; _mailbox_
- Generic Function: `mailboxp` _object_ &rarr; _result_
- Generic Function: `mailbox-address` _object_ &rarr; _address_
- Generic Function: `mailbox-display-name` _object_ &rarr; _result_
- Generic Function: `mailbox-local-part` _object_ &rarr; _result_
- Generic Function: `mailbox-domain` _object_ &rarr; _result_
- Generic Function: `mailbox-string` _object_ &rarr; _result_
