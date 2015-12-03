:- module(
  rdf_html_meta,
  [
    rdf_html//1, % :Content
    rdf_html_if_then//2, % :If
                         % :Then
    rdf_html_if_then_else//3, % :If
                              % :Then
                              % :Else
    rdf_html_ignore//1 % :Content
  ]
).

/** <module> RDF HTML meta

Conditional HTML content generation using RDF prefix expansion.

@author Wouter Beek
@version 2015/05, 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(rdf/rdf_read)).

:- html_meta(rdf_html(html,?,?)).
:- html_meta(rdf_html_if_then(0,html,?,?)).
:- html_meta(rdf_html_if_then_else(0,html,html,?,?)).
:- html_meta(rdf_html_ignore(html,?,?)).





%! rdf_html(:Content)// is det.

rdf_html(Content0) -->
  {rdf_global_term(Content0, Content)},
  html(Content).



%! rdf_html_if_then(:If, :Then)// is det.

rdf_html_if_then(If, Then) -->
  rdf_html_if_then_else(If, Then, html([])).



%! rdf_html_if_then_else(:If, :Then, :Else)// is det.

rdf_html_if_then_else(If0, Then0, Else0) -->
  {maplist(rdf_global_term, [If0,Then0,Else0], [If,Then,Else])},
  (   {call(If)}
  ->  Then
  ;   Else
  ).



%! rdf_html_ignore(:Content)// is det.

rdf_html_ignore(Content) -->
  html(Content), !.
rdf_html_ignore(_) --> [].
