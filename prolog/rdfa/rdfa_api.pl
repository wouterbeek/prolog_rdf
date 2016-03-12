:- module(
  rdfa_api,
  [
    rdfa_date_time//3,   % +P, +Something, +Masks
    rdfa_prefixed_iri/2, % +Iri, -PrefixedIri
    rdfa_prefixes/2      % +Aliases:list(atom), -Prefixes:atom
  ]
).

/** <module> RDFa build

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(date_time/date_time)).
:- use_module(library(html/html_date_time_human)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(http/html_write)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdfa/rdfa_api)).

:- rdf_meta
   rdfa_date_time(r, o, +, ?, ?).





%! rdfa_date_time(+P, +Something, +Masks)// is det.

rdfa_date_time(P1, Something, Masks) -->
  {
    something_to_date_time(Something, DT),
    date_time_masks(Masks, DT, MaskedDT),
    current_ltag(LTag),
    html_machine_date_time(MaskedDT, MachineString),
    xsd_date_time_datatype(DT, D1),
    maplist(rdfa_prefixed_iri, [P1,D1], [P2,D2])
  },
  html(
    time([datatype=D2,datetime=MachineString,property=P2],
      \html_human_date_time(MaskedDT, _{ltag: LTag, masks: Masks})
    )
  ).



%! rdfa_prefixed_iri(+Iri, -PrefixedIri) is det.

rdfa_prefixed_iri(Iri, PrefixedIri) :-
  rdf_global_id(Alias:Local, Iri),
  atomic_list_concat([Alias,Local], :, PrefixedIri).



%! rdfa_prefixes(+Aliases:list(atom), -Prefixes:atom) is det.

rdfa_prefixes(Aliases, Defs) :-
  maplist(rdf_current_prefix, Aliases, Prefixes),
  pairs_keys_values(Pairs, Aliases, Prefixes),
  maplist(pair_to_prefix, Pairs, Defs0),
  atomic_list_concat(Defs0, ' ', Defs).

pair_to_prefix(Alias-Prefix, Def) :-
  atomic_list_concat([Alias,Prefix], ': ', Def).
