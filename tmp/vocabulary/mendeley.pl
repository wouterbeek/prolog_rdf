:- module(
  mendeley,
  [
    mendeley_request/3 % +Category:oneof([document])
                       % +Query:list(nvpair)
                       % -Result:dict
  ]
).

/** <module> Mendeley API

GET https://api.mendeley.com/documents?view=bib

@author Wouter Beek
@version 2015/05
*/

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).

:- use_module(plc(generics/service_db)).




%! mendeley_request(
%!   +Category:oneof([document]),
%!   +Query:list(nvpair),
%!   -Result:dict
%! ) is det.

mendeley_request(Category, Query0, Result):-
  atomic_list_concat(['',Category], /, Path),
  uri_query_components(Query, Query0),
  uri_components(Uri, uri_components(https,'api.mendeley.com',Path,Query,_)),
  service(mendeley, token(Token)),
  atomic_list_concat(['Bearer',Token], ' ', Auth),
writeln(Uri),
  setup_call_cleanup(
    http_open(Uri, Out, [request_header('Authorization'=Auth)]),
    json_read_dict(Out, Result),
    close(Out)
  ).
