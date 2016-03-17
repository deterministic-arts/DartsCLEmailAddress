
DartsCLEmailAddress
===================

This library provides a fully RFC 5322 compliant parser for
email addresses. Also provided are a few tiny helper functions,
which allow the formatting of email addresses in ways, which
ensures, that they are RFC 5322 compliant.

Package DARTS.LIB.EMAIL-ADDRESS
-------------------------------

- Variable: `*allow-unicode*` 

  If true, the parser functions accept arbitrary characters (with `char-code` > 127)
  in addition to what they accept otherwise. This affects the productions
  of `ctext`, `atext`, `qtext`, and `dtext`. In other words: something like

    > `Däsiree Äßeldahl <d.äßeldahl@secret-äskulap.com>`

  becomes a valid email address. All base parser functions take a `:allow-unicode`
  keyword argument, whose default value is the value of this variable.

- Function: `parse-rfc5322-addr-spec` _string_ `&key` _start_ _end_ _allow-unicode_ _allow-trailing-junk_ &rarr; _local-part_ _domain_ _error_ _position_

  Parse _string_ (or a subequence of it) as an RFC 5322 `addr-spec`. If
  _allow-unicode_, characters outside of the ASCII range (i.e., with codes
  &gt; 127) are allowed virtually anywhere. See `*ALLOW-UNICODE*` for details,
  whose value also is the default for this argument.

  The values of _start_ and _end_ are bounding index designators for
  the part of _string_ to work on.

  If _allow-trailing-garbage_ is false (the default), the parser function
  makes sure, that no unprocessed characters remain in the designated input
  region of _string_ after a full address has successfully been parsed. If
  the value is true, this function does not check for unprocessed characters;
  the caller may inspect the returned _position_ value to determine, whether
  the string was processed fully, or whether unprocessed characters remain.

  This function returns four values:

  - _local-part_ is the value of the address' local part. If parsing fails
    early enough, this value is `nil`.

  - _domain_ is the value of the address' domain part. If parsing fails, 
    before the domain is encountered, this value is `nil`.

  - _error_ is a `nil`, if the string could be parsed successfully. Otherwise,
    it is a keyword symbol, which indicates, why the parser stopped.

  - _position_ is an integer, which identifies the last character in _string_
    successfully processed by this function.

- Function: `parse-rfc5322-mailbox` _string_ `&key` _start_ _end_ _allow-unicode_ _allow-trailing-junk_ &rarr; _local-part_ _domain_ _display-name_ _error_

  Parse _string_ (or a subequence of it) as an RFC 5322 `mailbox`. If
  _allow-unicode_, characters outside of the ASCII range (i.e., with codes
  &gt; 127) are allowed virtually anywhere. See `*ALLOW-UNICODE*` for details,
  whose value also is the default for this argument.

  The values of _start_ and _end_ are bounding index designators for
  the part of _string_ to work on.

  If _allow-trailing-garbage_ is false (the default), the parser function
  makes sure, that no unprocessed characters remain in the designated input
  region of _string_ after a full address has successfully been parsed. If
  the value is true, this function does not check for unprocessed characters;
  the caller may inspect the returned _position_ value to determine, whether
  the string was processed fully, or whether unprocessed characters remain.

  This function returns five values:

  - _local-part_ is the value of the address' local part. If parsing fails
    early enough, this value is `nil`.

  - _domain_ is the value of the address' domain part. If parsing fails, 
    before the domain is encountered, this value is `nil`.

  - _display-name_ is the display name found, or `nil`, if the address
   did not contain a display name part.

  - _error_ is a `nil`, if the string could be parsed successfully. Otherwise,
    it is a keyword symbol, which indicates, why the parser stopped.

  - _position_ is an integer, which identifies the last character in _string_
    successfully processed by this function.

- Function: `parse-rfc5322-mailbox-list` _string_ `&key` _start_ _end_ _allow-unicode_ &rarr; _list_ _error_ _position_

  Parse _string_ (or a subequence of it) as a comma separated list of RFC 
  5322 `mailbox` specifications. If _allow-unicode_, characters outside of the 
  ASCII range (i.e., with codes &gt; 127) are allowed virtually anywhere. 
  See `*ALLOW-UNICODE*` for details, whose value also is the default for 
  this argument.

  The values of _start_ and _end_ are bounding index designators for
  the part of _string_ to work on.

  If _allow-trailing-garbage_ is false (the default), the parser function
  makes sure, that no unprocessed characters remain in the designated input
  region of _string_ after a full address has successfully been parsed. If
  the value is true, this function does not check for unprocessed characters;
  the caller may inspect the returned _position_ value to determine, whether
  the string was processed fully, or whether unprocessed characters remain.

  This function returns three values:

  - _list_ is a list of sub-lists of the form `(local-part domain display-name)`,
    one sublist for each successfully parsed mailbox specification in the
    input string. The elements appear in the order, they are found in the
    input.

  - _error_ is a `nil`, if the string could be parsed successfully. Otherwise,
    it is a keyword symbol, which indicates, why the parser stopped.

  - _position_ is an integer, which identifies the last character in _string_
    successfully processed by this function.

- Function: `escape-local-part` _string_ `&key` _start_ _end_ &rarr; _result_

  Ensures, that _string_ is properly escaped for use as the local part of an
  email address. If necessary, this function adds quotes and backslashes. Note,
  that non-ASCII characters with codes > 127 are not special cased by this
  function, i.e., they are implicitly allowed.

  The values of _start_ and _end_ are bounding index designators for
  the part of _string_ to work on.

- Function: `escape-display-name` _string_ `&key` _start_ _end_ &rarr; _result_

  Ensures, that _string_ is properly escaped for use as the display name of a
  mailbox. If necessary, this function adds quotes and backslashes. Note,
  that non-ASCII characters with codes > 127 are not special cased by this
  function, i.e., they are implicitly allowed.

  The values of _start_ and _end_ are bounding index designators for the
  part of _string_ to work on.  

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

  A mailbox is basically an `address` combined with a display name.
  This class itself does not actually provide anything interesting. It
  merely exists for the purpose of type discrimination.

- Class: `basic-mailbox`

  This is a concrete implementation of the `mailbox` protocol. 
  Instances have two slots `mailbox-address` and `mailbox-display-name`.

- Function: `mailbox` _object_ &rarr; _mailbox_

  Tries to coerce its argument _object_ into an instance of class `mailbox`,
  according to the following rules:

    - if _object_ is already an instance of `mailbox`, it is returned directly

    - if _object_ is an `address`, a new `basic-mailbox` is created, whose address
      part is `object`, and whose display name is `nil`.

    - if _object_ is a string, it is parsed according to the RFC `mailbox`
      production, and the results are used to construct a new `basic-mailbox`.

  If this function cannot convert its argument into a `mailbox`, it signals
  an error of type `type-error`.

- Generic Function: `mailboxp` _object_ &rarr; _result_

  Tests, whether _object_ fulfills the `mailbox` protocol. This condition is 
  always true by definition for subclasses of class `mailbox`. It may additionally 
  be true for other objects.

- Generic Function: `mailbox-address` _object_ &rarr; _address_

  Answers the `address` instance, which describes the actual
  email address associated with mailbox _object_. This method is part of the
  core mailbox protocol, and must be implemented by all objects, which want
  to participate in that protocol.

- Generic Function: `mailbox-display-name` _object_ &rarr; _result_

  Answers the display name associated with the given mailbox
  instance _object_. This function is part of the core mailbox protocol and
  must be implemented by all objects, which want to participate in that 
  protocol.

- Generic Function: `mailbox-local-part` _object_ &rarr; _result_

  Answers the local part string of this mailbox's address.
  The default method simply extracts the `address-local-part` from the object
  returned by `mailbox-address` when applied to the given _object_.

- Generic Function: `mailbox-domain` _object_ &rarr; _result_

  Answers the domain string of this mailbox's address.
  The default method simply extracts the `address-domain` from the object
  returned by `mailbox-address` when applied to the given _object_.

- Generic Function: `mailbox-string` _object_ &rarr; _result_
  
  Constructs a string representation of the given mailbox
  instance. The result is required to be a well-formed RFC 5322 email address
  parsable using the `mailbox` production. The default method should be usable
  by almost all concrete `mailbox` implementations.
