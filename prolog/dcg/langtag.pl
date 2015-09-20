:- module(
  langtag,
  [
    'Language-Tag'//1 % ?LanguageTag:atom
  ]
).

/** <module> Language tags

# Case-sensitivity

Subtags are case-insensitive, but there are conventions for case:

  - [ISO639-1] recommends that language codes be written in lowercase
    ('mn' Mongolian).
  - [ISO15924] recommends that script codes use lowercase with the
    initial letter capitalized ('Cyrl' Cyrillic).
  - [ISO3166-1] recommends that country codes be capitalized ('MN'
    Mongolia).

---

@author Wouter Beek
@compat RFC 5646
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_abnf_rules)).
:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/langtag_char)).
:- use_module(library(lists)).
:- use_module(library(plunit)).





%! extension(?Subtags:list(atom))// .
% ```abnf
% extension = singleton 1*("-" (2*8alphanum))
% ```
%
% @compat RFC 5646
% @tbd If this subtag is case-insensitive then "X" should be excluded
%      as well?

extension([H|T]) -->
  singleton(H),
  (   "-",
      '+'(extension0, T, [separator(hyphen)])
  ;   {T = []}
  ).

extension0(T) -->
  dcg_atom('m*n'(2, 8, alphanum, []), T).



%! extlang(?Subtags:list(atom))// .
% Extended language subtag.
%
% ```abnf
% extlang = 3ALPHA           ; selected ISO 639 codes
%           *2("-" 3ALPHA)   ; permanently reserved
% ```
%
% @compat RFC 5646

extlang(L) -->
  '+n'(3, extlang0, L, [separator(hyphen)]).

extlang0(X) -->
  dcg_atom('#'(3, 'ALPHA', []), X).



%! grandfathered(?Subtags:list(atom))// .
% ```abnf
% grandfathered = irregular   ; non-redundant tags registered
%               / regular     ; during the RFC 3066 era
% ```
%
% @compat RFC 5646

grandfathered(L) --> irregular(L).
grandfathered(L) --> regular(L).



%! 'Language-Tag'(?LanguageTag:atom)// .
% ```abnf
% Language-Tag = langtag / privateuse / grandfathered
% ```
%
% @compat RFC 5646

'Language-Tag'(LTag) -->
  (   langtag(Subtags)
  ;   privateuse(Subtags)
  ;   grandfathered(Subtags)
  ),
  {atomic_list_concat(Subtags, -, LTag)}.



%! langtag(?Subtags:list(atom))// .
% ```abnf
% langtag = language
%           ["-" script]
%           ["-" region]
%           *("-" variant)
%           *("-" extension)
%           ["-" privateuse]
% ```
%
% @compat RFC 5646

langtag(L) -->
  language(L1),
  (   "-",
      script(X2),
      {L2 = [X2]}
  ;   {L2 = []}
  ),
  (   "-",
      region(X3),
      {L3 = [X3]}
  ;   {L3 = []}
  ),
  (   "-",
      *(variant, L4, [separator(hyphen)])
  ;   {L4 = []}
  ),
  (   "-",
      *(extension, L5, [separator(hyphen)])
  ;   {L5 = []}
  ),
  (   "-",
      privateuse(L6)
  ;   {L6 = []}
  ),
  {append([L1,L2,L3,L4,L5,L6], L)}.



%! language(?Subtags:list(atom))// .
% ```abnf
% language = 2*3ALPHA        ; shortest ISO 639 code, sometimes followed
%            ["-" extlang]   ; by extended language subtags
%          / 4ALPHA          ; or reserved for future use
%          / 5*8ALPHA        ; or registered language subtag
% ```
%
% @compat RFC 5646

language([H|T]) -->
  dcg_atom('m*n'(2, 3, 'ALPHA', []), H),
  (   "-",
      extlang(T)
  ;   ""
  ).
language(X) -->
  dcg_atom('#'(4, 'ALPHA', []), X).
language(X) -->
  dcg_atom('m*n'(5, 8, 'ALPHA', []), X).



%! irregular(?LanguageSubtags:list(atom))//
% Irregular tags do not match the langtag// production and would not
%  otherwise be considered 'well-formed'.
%
% These tags are all valid, but most are deprecated in favor of more modern
%  subtags or subtag combination.
%
% ```abnf
% irregular = "en-GB-oed"
%           / "i-ami"
%           / "i-bnn"
%           / "i-default"
%           / "i-enochian"
%           / "i-hak"
%           / "i-klingon"
%           / "i-lux"
%           / "i-mingo"
% ```
%
% @compat RFC 5646

irregular([en,gb,oed]) --> atom_ci('en-gb-oed').
irregular([i,ami]) --> atom_ci('i-ami').
irregular([i,bnn]) --> atom_ci('i-bnn').
irregular([i,default]) --> atom_ci('i-default').
irregular([i,enochian]) --> atom_ci('i-enochian').
irregular([i,hak]) --> atom_ci('i-hak').
irregular([i,klingon]) --> atom_ci('i-klingon').
irregular([i,lux]) --> atom_ci('i-lux').
irregular([i,mingo]) --> atom_ci('i-mingo').
irregular([i,navajo]) --> atom_ci('i-navajo').
irregular([i,pwn]) --> atom_ci('i-pwn').
irregular([i,tao]) --> atom_ci('i-tao').
irregular([i,tay]) --> atom_ci('i-tay').
irregular([i,tsu]) --> atom_ci('i-tsu').
irregular([sgn,be,fr]) --> atom_ci('sgn-be-fr').
irregular([sgn,be,nl]) --> atom_ci('sgn-be-nl').
irregular([sgn,ch,de]) --> atom_ci('sgn-ch-de').



%! privateuse(?Subtags:list(atom))// .
% ```abnf
% privateuse = "x" 1*("-" (1*8alphanum))
% ```
%
% @compat RFC 5646

privateuse(L) -->
  "x",
  (   "-",
      '+'(privateuse0(L), [separator(hyphen)])
  ;   {L = []}
  ).

privateuse0(X) -->
  dcg_atom('m*n'(1, 8, alphanum, []), X).



%! region(?Subtag:atom)// .
% ```abnf
% region =   2ALPHA   ; ISO 3166-1 code
%          / 3DIGIT   ; UN M.49 code
% ```
%
% @compat RFC 5646

region(X) -->
  dcg_atom('#'(2, 'ALPHA', []), X).
region(X) -->
  dcg_atom('#'(3, 'DIGIT', []), X).



%! regular(?Subtags:list(atom))// .
% These tags match the langtag// production, but their subtags are not
%  extended language or variant subtags: their meaning is defined by their
%  registration and all of these are deprecated in favor of a more modern
%  subtag or sequence of subtags.
%
% ```abnf
% regular = "art-lojban"
%         / "cel-gaulish"
%         / "no-bok"
%         / "no-nyn"
%         / "zh-guoyu"
%         / "zh-hakka"
%         / "zh-min"
%         / "zh-min-nan"
%         / "zh-xiang"
% ```
%
% @compat RFC 5646

regular([art,lojban]) --> atom_ci('art-lojban').
regular([cel,gaulish]) --> atom_ci('cel-gaulish').
regular([no,bok]) --> atom_ci('no-bok').
regular([no,nyn]) --> atom_ci('no-nyn').
regular([zh,guoyu]) --> atom_ci('zh-guoyu').
regular([zh,hakka]) --> atom_ci('zh-hakka').
regular([zh,min]) --> atom_ci('zh-min').
regular([zh,min,nan]) --> atom_ci('zh-min-nan').
regular([zh,xiang]) --> atom_ci('zh-xiang').



%! script(?Subtag:atom)// .
% ```abnf
% script = 4ALPHA   ; ISO 15924 code
% ```
%
% @compat RFC 5646

script(X) -->
  dcg_atom('#'(4, 'ALPHA', []), X).



%! variant(?Subtag:atom)// .
% Registered variants.
%
% ```abnf
% variant = 5*8alphanum / (DIGIT 3alphanum)
% ```
%
% @compat RFC 5646

variant(X) -->
  dcg_atom('m*n'(5, 8, alphanum, []), X).
variant(X) -->
  'DIGIT'(_, H),
  '#'(3, alphanum, T, []),
  {atom_codes(X, [H|T])}.





% UNIT TESTS

:- begin_tests(dcg_langtag_test).

test_langtag('zh').
test_langtag('zh-Latn').
test_langtag('zh-Latn-CN').
test_langtag('zh-Latn-CN-variant1').
test_langtag('zh-Latn-CN-variant1-a-extend1').
test_langtag('zh-Latn-CN-variant1-a-extend1-x-wadegile').
test_langtag('zh-Latn-CN-variant1-a-extend1-x-wadegile-private1').

test(dcg_langtag_parse, [forall(test_langtag(LTag))]):-
  once(atom_phrase('Language-Tag'(LTag), _)).

:- end_tests(dcg_langtag_test).
