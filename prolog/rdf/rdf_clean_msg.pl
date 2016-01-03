:- module(rdf_clean_msg, []).

/** <module> RDF clean message

@author Wouter Beek
@version 2015/10-2015/11
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(http/http_info)).
:- use_module(library(msg_ext)).

:- multifile(user:message_hook/3).





user:message_hook(Msg1, _, _) :-
  Msg1 = message(Msg2, _, _),
  Msg2 =.. [error|_],
  string_phrase(dcg_error(Msg2), S),
  msg_warning(S).



% Syntax error at position.
dcg_error(error(syntax_error(Descr),Pos)) -->
  pl_stream_position(Pos),
  " ",
  atom(Descr),
  nl.
% Procedure does not exist.
dcg_error(error(existence_error(procedure,Pred),context(CallingContext,_))) -->
  "Predicate ",
  pl_predicate(Pred),
  " does not exist within calling context ",
  pl_predicate(CallingContext),
  ".",
  nl.
% Socket error.
dcg_error(error(socket_error(Msg))) -->
  "Socket error: ",
  atom(Msg).
% No permission for IRI.
dcg_error(error(permission_error(url,Iri),context(_,status(Code,_)))) --> !,
  "No permission to download from IRI ",
  iri(Iri),
  ". ",
  http_status_code(Code),
  nl.
