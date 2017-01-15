:- module(document_epoch, [document_epoch/0]).

/** <module> Document epoch

Stores documents with the number of seconds since epoch in a TSV file.

@author Wouter Beek
@version 2015/12, 2016/04
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(date_time/date_time)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(http/rfc2616_header)).
:- use_module(library(lists)).
:- use_module(library(math/rational_ext)).
:- use_module(library(os/io)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_term)).
:- use_module(library(service/ll_api)).
:- use_module(library(sparql/sparql_query_client)).

%:- debug(document_epoch(processed)).
:- debug(last_modified(malformed)).
:- debug(sparql(request)).





document_epoch:-
  flag(document_epoch, _, 0),
  ll_endpoint(Iri, Opts1),
  merge_options(
    [
      default_graphs([
        'http://lodlaundromat.org#12',
        'http://lodlaundromat.org#seedlist',
        'http://lodlaundromat.org#metrics-12',
        'http://lodlaundromat.org/ontology#error',
        'http://lodlaundromat.org/ontology#http'
      ])
    ],
    Opts1,
    Opts2
  ),
  Query1 = '\c
PREFIX llm: <http://lodlaundromat.org/metrics/ontology/>\n\c
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n\c
SELECT (MAX(?f) AS ?max)\n\c
WHERE {\n\c
  ?doc llm:metrics ?met .\n\c
  ?met llm:degree ?deg .\n\c
  ?deg llm:mean ?mean .\n\c
  BIND(xsd:float(?mean) AS ?f)\n\c
}\n',
  once(sparql_select(Iri, Query1, Rows1, Opts)),
  Rows1 = [[MeanMaxLit]],
  rdf_lexical_form(MeanMaxLit, MeanMax),
  
  Query2 = '\c
PREFIX llm: <http://lodlaundromat.org/metrics/ontology/>\n\c
PREFIX llo: <http://lodlaundromat.org/ontology/>\n\c
PREFIX void: <http://rdfs.org/ns/void#>\n\c
SELECT ?md5 ?ext ?lmod ?term ?mean\n\c
WHERE {\n\c
  {\n\c
    ?doc llo:md5 ?md5 ;\n\c
         llo:statementsType ?type ;\n\c
         llm:metrics ?met .\n\c
    BIND(IF(STRSTARTS(?type,"triples"),"nt","nq") AS ?ext)\n\c
    ?met void:classes ?class ;\n\c
         void:properties ?props ;\n\c
         void:triples ?stmts ;\n\c
         llm:degree ?deg .\n\c
    BIND((?class+?props)/?stmts AS ?term)\n\c
    ?deg llm:mean ?mean .\n\c
    ?doc llo:lastModified ?lmod .\n\c
  } UNION {\n\c
    ?archive llo:containsEntry ?doc.\n\c
    ?archive llo:lastModified ?lmod .\n\c
    ?doc llo:md5 ?md5 ;\n\c
         llo:statementsType ?type ;\n\c
         llm:metrics ?met .\n\c
    BIND(IF(STRSTARTS(?type,"triples"),"nt","nq") AS ?ext)\n\c
    ?met void:classes ?class ;\n\c
         void:properties ?props ;\n\c
         void:triples ?stmts ;\n\c
         llm:degree ?deg .\n\c
    BIND((?class+?props)/?stmts AS ?term)\n\c
    ?deg llm:mean ?mean .\n\c
  }\n\c
}\n',
  call_to_stream('document_epoch.tsv.gz', document_epoch).

document_epoch(Out) :-
  ll_endpoint(Iri, Opts),
  forall(
    sparql_select(Iri, Query2, Rows2, Opts),
    maplist(store_row(Out, MeanMax), Rows2)
  ).


store_row(Out, MeanMax, [Md5Lit,ExtLit,LModLit,TermLit,MeanLit]):-
  document_epoch(LModLit, LMod),
  maplist(rdf_lexical_form, [Md5Lit,ExtLit,TermLit,MeanLit], [Md5,Ext,TermRat,Mean]),
  MeanRel is Mean / MeanMax,
  Term is float(TermRat),
  csv_write_stream(Out, [row(Md5,Ext,LMod,Term,MeanRel)], [separator(0'	)]),
  flag(document_epoch, N, N + 1),
  debug(document_epoch(processed), "Processed ~D documents.", [N]), !.


document_epoch(Lit, I):-
  rdf_literal_lexical_form(Lit, Lex),
  (   atom_phrase('last-modified'(DT), Lex)
  ->  timeOnTimeline(DT, S),
      rational_parts(S, I, _)
  ;   debug(last_modified(malformed), "Malformed Last-Modified value: ~a.", [Lex]),
      I = 0
  ), !.
document_epoch(_, 0):-
  debug(last_modified(missing), "Missing Last-Modified header.", []).
