# Prolog-based RDF library

This library provides advanced support for working with RDF in Prolog.

## Installation

Run the following command in [SWI-Prolog](https://www.swi-prolog.org):

```pl
pack_install(prolog_rdf).
```

## Use

Libraries can be loaded in the following way:

```pl
:- [library(rdf_term)].
```

## RDF-specific Prolog types

This library uses the following extended Prolog types in the
documentation headers of predicates:

| *Type*        | *Definition*                                                                         |
| ------------- | ------------------------------------------------------------------------------------ |
| `rdf_bnode`   | An atom that starts with `_:`.                                                       |
| `rdf_graph`   | Either a term of type `iri` or the atom `default`.                                   |
| `rdf_iri`     | An atom that can be decomposed with `uri_components/2` from `library(uri)`.          |
| `rdf_literal` | A compound term of the form `literal(type(iri,atom))` or `literal(lang(atom,atom))`. |
| `rdf_name`    | An RDF name (IRI or literal).                                                        |
| `rdf_quad`    | A compound term of the form `rdf(rdf_nonliteral,iri,rdf_term,rdf_graph)`.            |
| `rdf_term`    | An RDF term (blank node, IRI, or literal).                                           |
| `rdf_triple`  | A compound term of the form `rdf(rdf_nonliteral,iri,rdf_term)`.                      |
| `rdf_tuple`   | A term of type `rdf_quad` or `rdf_triple`.                                           |

## Modules

This section enumerates the various modules that are included in this
library.

### `library(rdf_clean)`

This module contains data cleaning predicates that were previously
part of [LOD Laundromat](http://lodlaundromat.org).  They can be used
to clean RDF tuples that are streamed from an RDF source.  See module
[[semweb/rdf_deref]] for creating streams over RDF sources.

In order to use this module, library
[`prolog_uriparser`](https://github.com/wouterbeek/prolog_uriparser)
must be installed.

#### Blank node cleaning

The parsers in the Semantic Web standard library emit blank node
labels that contain characters that are not allowed in
standards-compliant output formats (e.g., forward slashes).  This is
unfortunate, since writing the data into standard-compliant formats
requires maintaining a state that ensures that Prolog internal blank
node labels are consistently emitted by the same standard-compliant
external blank node label.  See [this Github
issue](https://github.com/SWI-Prolog/packages-semweb/issues/68) for
context.

Besides the above considerations, blank nodes form a scalability issue
in general.  Since blank node labels are only guaranteed to be unique
within the context of an RDF document, combining data from multiple
documents requires a check of all blank node labels in the to be
combined documents.  Furthermore, all blank node labels that appear in
more than one RDF document must be consistently renamed prior to
combining the data.

Since Pro-RDF focusses on scalability, it cannot rely on maintaining
an internal state that consistently maps internal Prolog blank node
labels to external standards-compliant blank node labels.  For the
same reasons, it also cannot rely on full document inspection and
blank node relabeling approaches.  For these reasons, the data
cleaning prediates in `library(rdf_clean)` replace blank nodes with
well-known IRIs, in line with the RDF 1.1 standard.  This means that
every data cleaning predicate must bind a valid well-known IRI to the
`BNodePrefix` argument.  It also means that Prolog internal blank node
labels are hashed using the MD5 algorithm to provide the local names
for the generated well-known IRIs.  The latter ensures consistent
relabeling without maintaining an internal state.

#### Graph cleaning

The parsers from the Semantic Web standard library denote the default
graph with atom `user`.  This is translated to atom `default`.  For
named graphs, this library checks whether they are well-formed IRIs.

#### IRI cleaning

IRI cleaning is the most difficult part of syntactic RDF data
cleaning.  To date, the IRI grammar ([RFC
3987](https://tools.ietf.org/html/rfc3987)) has not yet been
implemented.  Since this grammar was published over a decade ago, we
must anticipate a future in which the main syntactic component of the
Semantic Web cannot be validated.

While there are implementations of the URI grammar ([RFC
3986](https://tools.ietf.org/html/rfc3986)), the one provided by the
SWI-Prolog standard library (`library(uri)`) is incorrect.

Because of the above two reasons we currently only check the following:

  - Whether an IRI can be decomposed into scheme, authority, path,
    query, and fragment components using the Prolog standard library
    grammar (`uri_components/2`).

  - Whether the scheme, authority, and path components are non-empty.

  - Whether the scheme components conforms to the IRI grammar.

#### Literal cleaning

For language-tagged strings, cleaning involves downcasing the language
tag.  While there are implementations of the language tag grammar
([RFC 5646](https://tools.ietf.org/html/rfc5646)), we are not yet
using these.

Simple literals, i.e., literals with neither language tag not datatype
IRI, are translated to typed literals with datatype IRI `xsd:string`.

For typed literals, cleaning involves:

  - Cleaning the datatype IRI (see [[IRI cleaning]]).

  - Making sure the datatype IRI is not `rdf:langString`.

  - Cleaning the lexical form according to the datatype IRI.  Lexical
    form cleaning is the most involved step, since there are many
    different datatype IRIs.  Since it is impractical to implement
    lexical form cleaning for all datatype IRIs, we focus on those
    that are most widely used.  For this we use `rdf_literal_value/3`,
    which is part of library `library(semweb/rdf_term)`.

#### Predicates

This module provides the following predicates.

##### `rdf_clean_quad(+Site:uri, +Dirty:rdf_quad, -Clean:rdf_quad)`

Cleans quadruple compound terms.

##### `rdf_clean_triple(+Site:uri, +Dirty:rdf_triple, -Clean:rdf_triple)`

Cleans triple compound terms.

##### `rdf_clean_tuple(+Site:uri, +Dirty:rdf_tuple, -Clean:rdf_tuple)`

Cleans quadruple and/or triple compound terms.

### `library(rdf_deref)`

This module implements RDF dereferencing, i.e., the act of obtaining
interpreted RDF statements based on a given RDF document, stream, or
HTTP(S) URI.

#### Predicates

This library provides the following predicates.

##### `rdf_deref_file/[2,3]`

Calls RDF dereferencing on local RDF documents.  Uses heuristics in
order to determine the RDF serialization of the file.

##### `rdf_deref_stream/[3,4]`

Performs RDF dereferencing on an input stream containing one of the
standardized RDF serialization formats.

##### `rdf_dered_uri/[2,3]`

Performs RDF dereferencing on a URI, typically an HTTP(S) URI.  Uses
heuristics in order to determine the RDF serialization of the reply
body.

### `library(rdf_dot)`

This library provides primitives for generating GraphViz DOT exports
of RDF terms and tuples.

This module requires library
[`prolog_graphviz`](https://github.com/wouterbeek/prolog_graphviz) to
be installed.

### `library(rdf_export)`

This module writes RDF data in a simple and standards-compliant
serialization format.  It contains the following predicates:

  - `rdf_write_iri/2`
  - `rdf_write_literal/2`
  - `rdf_write_name/2`
  - `rdf_write_quad/[2,3,5]`
  - `rdf_write_triple/[2,4]`
  - `rdf_write_tuple/2`

### `library(rdf_guess)`

This module peeks at the beginning of a file, stream, or string in
order to heuristically guesstimate the RDF serialization formats (if
any) containing in that input:

  - `rdf_guess_file/3`
  - `rdf_guess_stream/3`
  - `rdf_guess_string/2`

### `library(rdf_media_type)`

This module provides support for the standardized RDF serialization
format Media Types:

#### `rdf_file_name_media_type/2`

Guesses the RDF serialization format based on the file name extension
alone.

#### `rdf_media_type/1`

Enumerates all standardized RDF Media Types.

#### `'rdf_media_type_>'/2`

Succeeds if the former argument is an RDF Media Type that
syntactically encompasses the latter argument (e.g., TriG > Turtle >
N-Triples, N-Quads > N-Triples).

#### `rdf_media_type_extension`

Gives a standard file name extension for RDF serializations that are
not RDFa (which is part of HTML or XHTML content).

#### `rdfa_media_type/1`

Succeeds for RDFa Media Types.

### `library(rdf_prefix)`

This module provides extended support for working with RDF prefix
declarations:

##### `rdf_prefix/[1,2]`

Enumerates the currently declared RDF prefix declarations.

#### `rdf_prefix_any/2`

#### `rdf_prefix_append/[2,3]`

#### `rdf_prefix_iri/[2,3]`

Succeeds for (alias,local-name) pairs and full IRIs.

#### `rdf_prefix_maplist/[2,3]`

#### `rdf_prefix_member/2`

#### `rdf_prefix_memberchk/2`

Provide the corresponding popular Prolog predicates, but apply RDF
prefix notation expansion on their arguments.

RDF prefix expansion must be specifically declared for arguments in
predicates.  In the SWI-Prolog standard libraries, such declarations
have only been added for predicates in the Semantic Web libraries, but
not for predicates in other standard libraries.  For example, the
following will not check whether `P` is bound to either of the four
RDFS properties, because the prefix notation is not expanded:

```pl
memberchk(P, [rdfs:domain,rdfs:range,rdfs:subClassOf,rdfs:subPropertyOf]),
```

With the SWI-Prolog standard library, the above call must be spelled
out using `rdf_equal/2` in the following way:

```pl
(   rdf_equal(P, rdfs:domain)
->  true
;   rdf_equal(P, rdfs:range)
->  true
;   rdf_equal(P, rdfs:subClassOf)
->  true
;   rdf_equal(P, rdfs:subPropertyOf)
->  true
)
```

When `library(rdf_prefix)` is loaded, the above can be written as
follows:

```pl
rdf_prefix_memberchk(P, [rdfs:domain,rdfs:range,rdfs:subClassOf,rdfs:subPropertyOf]),
```

#### `rdf_prefix_selectchk/3`

#### `rdf_prefix_term/2`

#### `rdf_register_prefix/[1-3]`

#### `rdf_register_prefixes/0`

### `library(rdf_print)`

This module provides DCG rules for printing RDF terms and tuples.

### `library(rdf_term)`

This module provides advanced support for composing, decomposing,
parsing, and generating RDF terms.
