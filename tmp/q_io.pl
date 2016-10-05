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

*/



%! q_create_void(+Refs, +D, :Goal_3, -G) is det.

q_create_void(Refs, D, Goal_3, G) :-
  q_void_iri(Refs, G),
  call(Goal_3, trp, D, G),
  % Write to store&view.
  q_view2store_overwrite(trp, G),
  q_store2view0(G).



%! q_graph_iri(+Refs, -G) is det.

q_graph_iri(Refs, G) :-
  q_alias_prefix(ns, Prefix),
  uri_components(Prefix, uri_components(Scheme,Auth,_,_,_)),
  q_abox_iri(Scheme, Auth, graph, Refs, G).



%! q_vocab_iri(?Refs, -G) is det.

q_vocab_iri(Refs, G) :-
  q_graph_iri([vocab|Refs], G).



%! q_void_iri(+Refs, -G) is det.

q_void_iri(Refs, G) :-
  q_graph_iri([void|Refs], G).
