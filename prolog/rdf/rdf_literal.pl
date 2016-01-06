:- module(
  rdf_literal,
  [
    rdf_is_language_tagged_string/1, % @Term
    rdf_language_tagged_string/1, % ?Lit
    rdf_literal_components/4, % ?Lit, ?D, ?Lex, ?LTag
    rdf_literal_data/3, % ?Field, +Lit, ?D
    rdf_literal_equiv/2 % +Lit1, +Lit2
  ]
).
:- reexport(library(rdf11/rdf11), [
     rdf_lexical_form/2 % +Lit, -Lex
   ]).

/** <module> RDF literal

@author Wouter Beek
@version 2015/08-2015/09, 2015/11-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_legacy)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).

:- rdf_meta
   rdf_language_tagged_string(o),
   rdf_literal_components(o, r, -, -),
   rdf_literal_equiv(o, o).





%! rdf_is_language_tagged_string(@Term) is semidet.
% Succeeds on language-tagged strings.
%
% The **language-tagged string**s are the cartesian product of the Unicode
% strings in Normal Form C with the set of BCP 47 language tags.

rdf_is_language_tagged_string(Lit) :-
  ground(Lit),
  Lit = _@_.



%! rdf_language_tagged_string(-Lit) is semidet.
%! rdf_language_tagged_string(-Lit) is nondet.

rdf_language_tagged_string(Lit) :-
  rdf_literal(Lit),
  rdf_is_language_tagged_string(Lit).



%! rdf_literal_components(+Lit, -D, -Lex, -LTag) is det.
%! rdf_literal_components(-Lit, +D, +Lex, +LTag) is det.
%! rdf_literal_components(+Lit, +D, +Lex, -LTag) is det.
%! rdf_literal_components(+Lit, -D, +Lex, -LTag) is det.

rdf_literal_components(V@LTag, rdf:langString, V, LTag) :-
  atom(LTag).
rdf_literal_components(V^^D,   D,              V, LTag) :-
  rdf_is_iri(D),
  var(LTag).



%! rdf_literal_data(+Field, +Lit, +Data) is semidet.
%! rdf_literal_data(+Field, +Lit, -Data) is det.
%! rdf_literal_data(-Field, +Lit, -Data) is multi.
% Decomposes literals.
%
% Field is one of:
%   - `datatype`
%   - `langtag`
%   - `lexical_form`
%   - `value`
%
% @tbd How to derive the lexical form from `rdf11'?
% @throws type_error

rdf_literal_data(datatype, _^^D, D) :- !.
rdf_literal_data(datatype, _@_, rdf:langString) :- !.
rdf_literal_data(langtag, _@LTag, LTag) :- !.
rdf_literal_data(lexical_form, Lit, Lex) :- !,
  rdf11:pre_object(Lit, Lit0),
  rdf_legacy_literal_components(Lit0, _, Lex, _).
rdf_literal_data(value, V^^_, V) :- !.
rdf_literal_data(value, V@_, V).
rdf_literal_data(Field, _, _) :-
  must_be(oneof([datatype,langtag,lexical_form,value]), Field).



%! rdf_literal_equiv(+Lit1, +Lit2) is semidet.
% Succeeds if the given literals are equivalent.
%
% Two literals are equivalent if:
%   1. The strings of the two lexical forms compare equal,
%      character by character.
%   2. Either both or neither have language tags.
%   3. The language tags, if any, compare equal.
%   4. Either both or neither have datatype URIs.
%   5. The two datatype URIs, if any, compare equal, character by character.
%
% @compat [RDF 1.0 Concepts and Abstract Syntax](http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/)
% @tbd Update to RDF 1.1.

% Equivalent language-tagged strings.
rdf_literal_equiv(Lex@LTag1, Lex@LTag2) :- !,
  downcase_atom(LTag1, LTag),
  downcase_atom(LTag2, LTag).
% Equivalent typed literals have the same datatype and
% have equivalent values in the datatype's value space.
rdf_literal_equiv(V1^^D, V2^^D) :-
  V1 == V2.
