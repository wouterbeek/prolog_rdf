:- module(
  rdf_prefixes,
  [
    assert_data_prefixes/0,
    assert_prefixes/0,
    assert_schema_prefixes/0
  ]
).

/** <module> RDF prefixes

RDF prefix registrations.

# Reduced

A **reduced location** is an IRI whose prefix is a reduced location prefix.

A **reduced location prefix** is a registered RDF prefix
all of whose reduced locations should be dereferenced as their prefix.

Reduced locations often occur in vocabularies,
where the file denoted by [1] contains the assertions for [2], [3], etc.

```uri
[1]   http://www.w3.org/1999/02/22-rdf-syntax-ns#
[2]   http://www.w3.org/1999/02/22-rdf-syntax-ns#first
[3]   http://www.w3.org/1999/02/22-rdf-syntax-ns#type
```

---

@author Wouter Beek
@tbd Add prefixes that occur in http://dbpedia.org/sparql?nsdecl
@version 2014-2015
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(uri)).

:- use_module(plHttp(download_to_file)).

:- use_module(plRdf(management/rdf_prefix)).





assert_data_prefixes:-
  assert_prefixes(data).

assert_schema_prefixes:-
  assert_prefixes(schema).

assert_prefixes:-
  assert_data_prefixes,
  assert_schema_prefixes.

%! assert_prefixes(+Type:oneof([data,schema])) is det.

assert_prefixes(Type):-
  absolute_file_name(plRdf('prefixes.csv'), File, [access(read)]),
  csv_read_file(File, Rows),
  forall(
    member(row(Uri,Prefix,Type), Rows),
    rdf_reset_prefix(Prefix, Uri)
  ).
