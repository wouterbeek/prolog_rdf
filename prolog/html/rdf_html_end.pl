:- module(
  rdf_html_end,
  [
    rdf_html_literal_end//2, % +Lit, +Opts
    rdf_html_property_end//2 % +Prop, +Opts
  ]
).

/** <module> RDF HTML

Generates end user-oriented HTML representations of RDF data.

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(html/rdf_html_term)).
:- use_module(library(http/html_write)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf11/rdf11)).

:- rdf_meta
   rdf_html_literal_end(o, +, ?, ?).



rdf_html_literal_end(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  html(V).
rdf_html_literal_end(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html('~D'-[V]).
rdf_html_literal_end(Lit, Opts) -->
  rdf_html_literal(Lit, Opts).



rdf_html_property_end(Prop, Opts) -->
  html([
    a(href=Prop, \rdf_html_property_end0(Prop, Opts)),
    a(href=Prop, span(['aria-hidden'=true,class=[glyphicon,'glyphicon-link']],[]))
  ]).

rdf_html_property_end0(Prop, _) --> {rdfs_label(Prop, Lbl)}, !, html(Lbl).
rdf_html_property_end0(Prop, Opts) --> rdf_html_predicate(Prop, Opts).
