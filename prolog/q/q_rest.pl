:- module(
  q_rest,
  [
    q_rest_handler/5 % +Request, +HandleId, +Class, :Singular_3, :Plural_2
  ]
).
:- reexport(library(http/rest)).

/** <module> Quine REST call

Support for RESTful interfaces to RDF data.

@author Wouter Beek
@version 2016/02, 2016/04-2016/07
*/

:- use_module(library(semweb/rdf11)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(yall)).

:- meta_predicate
   q_rest_handler(+, +, +, 3, 2).

:- rdf_meta
   q_rest_handler(+, +, r, :, :).





q_rest_handler(Req, HandleId, C, Singular_3, Plural_2) :-
  rest_handler(
    Req,
    HandleId,
    {C}/[I]>>rdfs_instance(I, C),
    Singular_3,
    Plural_2
  ).
