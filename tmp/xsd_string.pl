:- module(
  xsd_string,
  [
    stringCanonicalMap//1, % +String:atom
    stringLexicalMap//1 % -String:atom
  ]
).

/** <module> XSD string datatype

```ebnf
stringRep ::= Char*
```

The **`string`** datatype represents character strings in XML.

# Value space

The set of finite-length sequences of characters that match the xml_char//2
production from either XML 1.0 or XML 1.1.

# Lexical mapping

The set of finite-length sequences of zero or more characters that match the
xml_char//2 production from XML 1.0 or XML 1.1.

---

@author Wouter Beek
@compat XSD 1.1
@see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/)
@version 2015/07, 2015/11
*/

:- use_module(library(dcg_ext)).
:- use_module(library(xml/xml11_code)).






stringCanonicalMap(A) -->
  dcg_atom(*('Char'), A).



stringLexicalMap(A) -->
  dcg_atom(*('Char'), A).
