:- module(
  rdf_rest,
  [
    rdf_rest_handler/5 % +Request, +HandleId, +Class, :Singular_3, :Plural_2
  ]
).

:- reexport(library(http/rest)).

/** <module> RDF REST

Support for RESTful interfaces to RDF data.

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(rdf/rdf_api)).
:- use_module(library(yall)).

:- meta_predicate
   rdf_rest_handler(+, +, +, 3, 2).

:- rdf_meta
   rdf_rest_handler(+, +, r, :, :).



rdf_rest_handler(Req, HandleId, C, Singular_3, Plural_2) :-
  rest_handler(Req, HandleId, [I]>>rdfs_instance0(I, C), Singular_3, Plural_2).
