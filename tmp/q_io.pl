:- module(
  q_io,
  [
    % CREATE
    q_create/0,
    q_create/1,       % +Name
    q_create_vocab/3, % +Refs, :Goal_2, -G
    q_create_void/4,  % +D, +Refs, :Goal_3, -G
    
    % GENERICS
    q_graph_iri/2,    % ?Refs, ?G
    q_vocab_iri/2,    % ?Refs, -G
    q_void_iri/2      % ?Refs, -G
  ]
).

/** <module> Quine input/output

Design principles:

  1. _any_ file can be uploaded, starting with:

    a. CSV

    b. Turtle 1.1, N-Triples 1.1, N-Quads 1.1, TRiG 1.1

    c. RDFa

    d. JSON, NDJSON, JSONLD

    e. Shapefile

    f. XML, RDF/XML 1.1

  2. over _any_ protocol, starting with:

    a. file

    b. HTTP(S) download

  3. in _any_ archive format, starting with:

    a. 7zip, ar, cab, cpio, empty, GNU Tar, ISO-9660, lha, RAR, raw,
       tar, xar, zip

  4. under _any_ compression filter, starting with:

    a. bzip2, compress, gzip, grzip, lrzip, lzip, lzma, lzop, none,
       rpm, uu, xz

  5. If the file contains graphs then these are store into independent
     files.  If the file does not contain graphs then all data is
     stored in one graph.

  6. Datasets are defined as non-empty lists of graph pointers.

  7. Each graph has an IRI and a file name.  IRI and file name have a
     one-to-one mapping.

---

Source Layer

↓ q_source2store/[0,2,3]

Storage Layer

↓ q_store2cache/[0-2]

Cache Layer

↓ q_cache2view/[1,2]

View Layer

---

Purpose of each layer

  - The source layer contains the raw sources that can be in any (also
    non-RDF) format.

  - The storage layer contains the converted data stored in a single,
    clean and standards-compliant RDF format.

  - The cache layer contains the data cached in a format for use in
    specific views.  This is not necessarily RDF, since that is not
    optimal for most applications.

  - The view layer contains data loaded for direct use in
    applications.

---

The supported source formats are extended through hooks:

  - q_source_format_hook(?Format, -Exts)

---

The supported cache formats are extended through hooks:

  - q_store2cache_hook(+M, +G)

  - q_cache2view_hook(+M, +G)

  - q_cache_format_hook(?M, -Exts)

  - q_cache_rm_hook(+M, +G)

  - q_view_rm_hook(+M, +G)

---

The following flags are used:

  * q(q_io)

---

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(q/q_dataset)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).

:- meta_predicate
    q_create_vocab(+, 2, -),
    q_create_void(+, +, 3, -).

:- multifile
    q_cache2view_hook/2, % E.g., open HDT file.
    q_store2cache_hook/2, % E.g., create HDT file from N-Triples file.
    q_view_rm_hook/2. % E.g., remove the HDT files for specific graphs.

:- rdf_meta
   q_cache2view(+, r),
   q_cache_graph(-, r),
   q_change_cache(+, r, +),
   q_create(r),
   q_create_void(r, +, :, -),
   q_graph_iri(+, r),
   q_store2cache(+, r),
   q_store2view(+, r),
   q_store_file(?, r),
   q_store_graph(r),
   q_transform(r, :),
   q_transform(+, +, r, :),
   q_view2store_overwrite(+, r),
   q_view2store_append(+, r),
   q_view_rm(+, r),
   q_view_graph(-, r),
   q_vocab_iri(?, r),
   q_void_iri(?, r).





% CREATE %

%! q_create_vocab(+Refs, :Goal_2, -G) is det.

q_create_vocab(Refs, Goal_2, G) :-
  q_vocab_iri(Refs, G),
  call(Goal_2, rdf, G),
  % Write to store&view.
  q_view2store_overwrite(rdf, G),
  q_store2view0(G).



%! q_create_void(+Refs, +D, :Goal_3, -G) is det.

q_create_void(Refs, D, Goal_3, G) :-
  q_void_iri(Refs, G),
  call(Goal_3, rdf, D, G),
  % Write to store&view.
  q_view2store_overwrite(rdf, G),
  q_store2view0(G).




% GENERICS %

%! q_graph_iri(+Refs, -G) is det.

q_graph_iri(Refs, G) :-
  q_alias_domain(ns, Domain),
  q_abox_iri(Domain, graph, Refs, G).



%! q_vocab_iri(?Refs, -G) is det.

q_vocab_iri(Refs, G) :-
  q_graph_iri([vocab|Refs], G).



%! q_void_iri(+Refs, -G) is det.

q_void_iri(Refs, G) :-
  q_graph_iri([void|Refs], G).
