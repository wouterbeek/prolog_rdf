:- module(
  lotus_api,
  [
    lotus/2,      % +Query, -S
    lotus/3,      % +Query, -S, +Opts
    lotus/5,      % ?S, ?P, +Query, -O, -Doc
    lotus/6,      % ?S, ?P, +Query, -O, -Doc, +Opts
    match_term/1, % ?MatchTerm
    rank_term/1   % ?RankTerm
  ]
).

/** <module> LOTUS (Linked Open Text UnleaShed) API

@author Wouter Beek
@author Filip Ilievski
@see http://lotus.lodlaundromat.org/
@tbd Add support for hyper-parameters.
@version 2015/10, 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(debug)).
:- use_module(library(json_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(uri)).

:- setting(
     endpoint_host,
     atom,
     'lotus.lodlaundromat.org',
     "The host name of the LOTUS endpoint."
   ).

:- setting(
     endpoint_scheme,
     oneof([http,https]),
     http,
     "The scheme of the LOTUS endpoint."
   ).


:- rdf_meta
   lotus(r, r, +, -, -),
   lotus(r, r, +, -, -, +).





%! lotus(+Query:string, -S) is nondet.
%! lotus(+Query:string, -S, +Opts) is nondet.
%! lotus(?S, ?P, +Query:string, -O, -Doc) is nondet.
%! lotus(?S, ?P, +Query:string, -O, -Doc, +Opts) is nondet.
%
% The following options are supported:
%
%   * blank(+boolean) Whether or not results in which the subject term
%   is a LOD Laundromat blank node should be included or not.  LOD
%   Laundromat blank nodes are IRIs with prefix
%   `http://lodlaundromat.org/.well-known/genid/`.  The default value
%   is `false`.
%
%   * ltag(+atom) Filter by language tag.
%
%   * ltag_mode(+oneof([auto,other,user])) Whether user-supplied
%   (`user`, default), automatically tagged (`auto`) or unannotated
%   (`other`, ?) values for option `langtag` should be used.
%
%   * match(+atom)
%     The following values are supported:
%
%       * conjunct Matches all terms, not necessarily in the same
%       order.
%
%       * fuzzyconjunct
%
%       * phrase This is the default.
%
%       * terms Does not require every word to be matched.  This is
%       rather slow.
%
%   * rank(+atom) The following values are supported:
%
%       * degree
%
%       * lengthnorm
%
%       * numdocs
%
%       * proximity
%
%       * psf This is the default.
%
%       * recency
%
%       * semrichness
%
%       * termrichness
%
%   * request_size(+nonneg) Default is 10.
%
%   * result_size(-nonneg)
%
%   * result_time(-nonneg) In miliseconds.
%
% The following LOTUS options are currently not supported:
%
%   * predicate(+atom) Filter by (part of) a predicate IRI.  Wildcards
%   and Boolean operators can be used.
%
%   * subject(+atom) Filter by (part of) a subject IRI.  Wildcards and
%   Boolean operators can be used.

lotus(Query, S) :-
  lotus(Query, S, []).


lotus(Query, S, Opts) :-
  distinct(S, lotus(S, _, Query, _, _, Opts)).


lotus(S, P, Query, O, Doc) :-
  lotus(S, P, Query, O, Doc, []).


lotus(S, P, Query, O, Doc, Opts) :-
  setting(lotus:endpoint_scheme, Scheme),
  setting(lotus:endpoint_host, Host),
  option(blank(Blank), Opts, false),
  boolean_negation(Blank, NoBlank),
  option(ltag(LTag0), Opts, _),
  option(ltag_mode(LTagMode), Opts, user),
  option(match(Match), Opts, phrase),
  match_term(Match),
  option(rank(Rank), Opts, psf),
  rank_term(Rank),
  option(request_size(RequestSize), Opts, 10),
  QueryComps0 = [
    noblank(NoBlank),
    langannotator(LTagMode),
    langtag(LTag0),
    match(Match),
    predicate(P),
    rank(Rank),
    size(RequestSize),
    string(Query),
    subject(S)
  ],
  include(ground, QueryComps0, QueryComps),
  uri_query_components(QueryComp, QueryComps),
  uri_components(Iri, uri_components(Scheme,Host,'/retrieve',QueryComp,_)),
  debug(lotus(request), "REQUEST: ~a~n", [Iri]),
  json_read_any(Iri, Results),
  ignore(option(result_size(Results.numhits), Opts)),
  ignore(option(result_time(Results.took), Opts)),
  debug(lotus(response), "RESPONSE: ~D hits in ~Dms", [Results.numhits,Results.took]),
  member(Result, Results.hits),
  maplist(
    atom_string,
    [S,P,Lex,LTag,Doc],
    [Result.subject,Result.predicate,Result.string,Result.langtag,Result.docid]
  ),
  (ground(LTag) -> O = Lex@LTag ; O = Lex^^xsd:string).



%! match_term(+Term) is semidet.
%! match_term(-Term) is multi.

match_term(conjunct).
match_term(fuzzyconjunct).
match_term(phrase).
match_term(terms).



%! rank_term(+Term) is semidet.
%! rank_term(-Term) is multi.

rank_term(lengthnorm).
rank_term(proximity).
rank_term(psf).
rank_term(recency).
rank_term(semrichness).
rank_term(termrichness).





% HELPERS %

boolean_negation(true, false).
boolean_negation(false, true).



%! ll_is_bnode(@Term) is semidet.

ll_is_bnode(B) :-
  atom_prefix(B, 'http://lodlaundromat.org/.well-known/genid/').
