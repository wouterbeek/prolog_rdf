:- module(
  lotus_api,
  [
    lotus/2,         % +Query, -Iri
    lotus/3,         % +Query, -Iri, +Opts
    lotus_result/2,  % +Query, -Dict
    lotus_result/3,  % +Query, -Dict, +Opts
    lotus_results/2, % +Query, -Dict
    lotus_results/3, % +Query, -Dict, +Opts
    match_term/1,    % ?MatchTerm
    rank_term/1      % ?RankTerm
  ]
).

/** <module> LOTUS (Linked Open Text UnleaShed) API

@author Wouter Beek
@author Filip Ilievski
@see http://lotus.lodlaundromat.org/
@tbd Add support for hyper-parameters.
@version 2015/10, 2016/07-2016/08, 2016/10
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(debug)).
:- use_module(library(json_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(uri)).

:- setting(
     host,
     atom,
     'lotus.lodlaundromat.org',
     "The host name of the LOTUS endpoint."
   ).
:- setting(
     scheme,
     oneof([http,https]),
     http,
     "The scheme of the LOTUS endpoint."
   ).





%! lotus(+Query:string, -Iri) is nondet.
%! lotus(+Query:string, -Iri, +Opts) is nondet.
%
% The following options are supported:
%
%   * blank(+boolean)
%
%     Whether or not results in which the subject term is a LOD
%     Laundromat blank node should be included or not.  LOD Laundromat
%     blank nodes are IRIs with prefix
%     `http://lodlaundromat.org/.well-known/genid/`.  The default
%     value is `false`.
%
%   * ltag(+atom)
%
%     Filter by language tag.
%
%   * ltag_mode(+oneof([auto,other,user]))
%
%     Whether user-supplied (`user`, default), automatically tagged
%     (`auto`) or unannotated (`other`, ?) values for option `ltag`
%     should be used.
%
%   * match(+atom)
%
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
%   * rank(+atom)
%
%     The following values are supported:
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
%   * request_size(+nonneg)
%
%     Default is 10.
%
%   * result_size(-nonneg)
%
%   * result_time(-nonneg)
%
%     In miliseconds.
%
% The following LOTUS options are currently not supported:
%
%   * predicate(+atom)
%
%     Filter by (part of) a predicate IRI.  Wildcards and Boolean
%     operators can be used.
%
%   * subject(+atom)
%
%     Filter by (part of) a subject IRI.  Wildcards and Boolean
%     operators can be used.

lotus(Query, Iri) :-
  lotus(Query, Iri, []).


lotus(Query, Iri, Opts) :-
  distinct(Iri, (
    lotus_result(Query, Dict, Opts),
    atom_string(Iri, Dict.subject)
  )).



%! lotus_result(+Query, -Dict) is nondet.
%! lotus_result(+Query, -Dict, +Opts) is nondet.

lotus_result(Query, Dict) :-
  lotus_result(Query, Dict, []).


lotus_result(Query, Dict, Opts) :-
  lotus_results(Query, Dict0, Opts),
  member(Dict, Dict0.hits).



%! lotus_results(+Query, -Dict) is nondet.
%! lotus_results(+Query, -Dict, +Opts) is nondet.

lotus_results(Query, Dict) :-
  lotus_results(Query, Dict, []).


lotus_results(Query, Dict, Opts) :-
  setting(scheme, Scheme),
  setting(host, Host),
  option(blank(Blank), Opts, false),
  boolean_negation(Blank, NoBlank),
  option(ltag(LTag0), Opts, _),
  option(ltag_mode(LTagMode), Opts, user),
  option(match(Match), Opts, phrase),
  match_term(Match),
  option(rank(Rank), Opts, psf),
  rank_term(Rank),
  option(request_size(RequestSize), Opts, 10),
  ignore(option(predicate(P), Opts)),
  ignore(option(subject(S), Opts)),
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
  Query \== "",
  include(ground, QueryComps0, QueryComps),
  uri_query_components(QueryComp, QueryComps),
  uri_components(Iri, uri_components(Scheme,Host,'/retrieve',QueryComp,_)),
  json_read_any(Iri, Dict).



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
