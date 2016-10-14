plXsd
=====

Support for XML Schema 1.1 Datatypes (XSD) in SWI-Prolog.

This library allows you to, for instance, share date-time values
between a Prolog and a Python process (even though Prolog and
Python use different internal representations for date-time values).

Because XSD allows you to interchange typed values
in an implementation-independent manner it is also used
in RDF for serializing RDF literals
(see library [plRdf](http://github.com/wouterbeek/plRdf.git)).



Installation
------------

  1. Install [SWI-Prolog](http://www.swi-prolog.org/Download.html).
  2. Run the following from the SWI-Prolog top-level:
  
     ```prolog
     ?- pack_install(plXsd).
     ```



Mappings
--------

Each XML Schema datatype consist of the following three components:

  1. A lexical space containing *expressions* that are
     representations of values in XML notation.
  2. A value space containing *values*.
  3. Functions defined on those spaces.

This library implements the following functions:

  1. Use `xsd_lexical_map/3` to parse an XML expression
     into a Prolog value.
     For example, both `"True"` and `"1"` are parsed as
     the Prolog boolean `true`.
  2. Use `xsd_canonical_map/3` to generate an XML expression
     for a given Prolog value.
     For example, the Prolog boolean `true` is canonically
     denoted by the XML expression `"True"`.

![Overview of XSD mappings](https://raw.githubusercontent.com/wouterbeek/plXsd/master/img/mapping.png)



Datatypes
---------

The following datatypes are implemented:

| *XSD datatype*     | *Prolog type* |
|:-------------------|:--------------|
| anyURI             | atom          |
| boolean            | boolean       |
| date               | date          |
| dateTime           | date          |
|*dayTimeDuration    | date          |
| decimal            | rational      |
| double             | float         |
|*duration           | date          |
| float              | float         |
| gDay		     | date    	     |
| gMonth	     | date          |
| gMonthDay          | date	     |
| gYear		     | date	     |
| gYearMonth	     | date	     |
|*hexBinary	     | ???	     |
| integer	     | integer	     |
| nonNegativeInteger | nonneg	     |
| string	     | atom	     |
| time		     | date	     |
|*yearMonthDuration  | date	     |

Notice that the datatype is crucial for the way in which
a lexical expression is interpreted!
For instance the XML notation `0.3333333333333333` denotes
a different value when interpreter as a float than when
interpreterd as a decimal:

```prolog
?- xsd_lexical_map(xsd:float, '0.3333333333333333', X), Y is X * 3.
X = 0.3333333333333333,
Y = 1.0.

?- xsd_lexical_map(xsd:decimal, '0.3333333333333333', X), Y is X * 3.
X = 3333333333333333 rdiv 10000000000000000,
Y = 9999999999999999 rdiv 10000000000000000.
```

---

This library was programmed by Wouter Beek in 2014-2015
and is distributed under the MIT License.
