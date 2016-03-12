:- module(
  jsonld_generics,
  [
    jsonld_abbreviate_iri/3, % +Context, +Full, -Compact
    jsonld_expand_term/3,     % +Context, +Compact, -Full
    jsonld_is_bnode/1,       % +Term
    jsonld_keyword/1         % ?Keyword
  ]
).

/** <module> JSON-LD generics

@author Wouter Beek
@version 2016/01-2016/02
*/

:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).





%! jsonld_abbreviate_iri(+Context, +Full, -Compact) is det.

jsonld_abbreviate_iri(Context, Full, Compact) :-
  is_iri(Full),
  get_dict(Alias, Context, Prefix),
  atom_concat(Prefix, Local, Full), !,
  atomic_list_concat([Alias,Local], :, Compact).



%! jsonld_expand_term(+Context, +Compact, -Full) is det.

% Case 0: IRIs are stored as strings in JSON-LD, so first turn them into atoms.
jsonld_expand_term(Context, Compact1, Full) :-
  string(Compact1), !,
  atom_string(Compact2, Compact1),
  jsonld_expand_term(Context, Compact2, Full).
% Case 1: An emty IRI expands to the base IRI.
jsonld_expand_term(Context, '', Full) :-
  get_dict('@base', Context, Full).
% Case 2: Do not expand blank nodes.
jsonld_expand_term(_, B, B) :-
  rdf_is_bnode(B).
% Case 3: Do not expand JSON-LD keywords.
jsonld_expand_term(_, Keyword, Keyword) :-
  jsonld_keyword(Keyword), !.
% Case 4: Aliased names must be expanded using the context.
jsonld_expand_term(Context, Compact, Full) :-
  atomic_list_concat([Alias,Local], :, Compact),
  \+ atom_prefix(Local, '//'),
  get_dict(Alias, Context, Prefix), !,
  atomic_concat(Prefix, Local, Full).
% Case 5: Non-aliased names that can be expanded using the context:
%         Either an IRI string or an object describing the IRI string
%         in addition to type coersion.
jsonld_expand_term(Context, Alias, Full) :-
  get_dict(Alias, Context, Def),
  (   string(Def)
  ->  jsonld_expand_term(Context, Def, Full)
  ;   get_dict('@id', Def, Full)
  ), !.
% Case 6: Names that cannot be expanded belong to a vocabulary IRI, if present.
%         This excludes names that are explicitly mapped to ‘null’ by the
%         context.
jsonld_expand_term(Context, Compact, Full) :-
  get_dict('@vocab', Context, Vocab),
  \+ jsonld_is_null(Context, Compact), !,
  atomic_concat(Vocab, Compact, Full).
% Case 7: Resolve relative IRIs WRT the base IRI.
jsonld_expand_term(Context, Compact, Full) :-
  get_dict('@base', Context, Base), !,
  uri_resolve(Compact, Base, Full).
% Case 8: No expansion / already a full IRI.
jsonld_expand_term(_, Full, Full) :-
  is_http_iri(Full), !.



%! jsonld_is_bnode(+Term) is semidet.

jsonld_is_bnode(T) :-
  atomic_list_concat(['_',_], :, T).



%! jsonld_is_null(+Context, +X) is semidet.

jsonld_is_null(Context, X) :-
  get_dict(map, Context, Map),
  memberchk(X-null, Map).



%! jsonld_keyword(+Key) is semidet.
%! jsonld_keyword(-Key) is multi.

jsonld_keyword('@base').
jsonld_keyword('@context').
jsonld_keyword('@graph').
jsonld_keyword('@id').
jsonld_keyword('@type').
jsonld_keyword('@value').
