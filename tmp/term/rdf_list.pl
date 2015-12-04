:- module(
  rdf_list,
  [
    rdf_list_after/2, % ?After:rdf_term
                      % ?Before:rdf_term
    rdf_list_after/4, % ?After:rdf_term
                      % ?Before:rdf_term
                      % ?List:or([bnode,iri])
                      % ?Graph:atom
    rdf_list_before/2, % ?Before:rdf_term
                       % ?After:rdf_term
    rdf_list_before/4, % ?Before:rdf_term
                       % ?After:rdf_term
                       % ?List:or([bnode,iri])
                       % ?Graph:atom
    rdf_list_directly_after/4, % ?After:rdf_term
                               % ?Before:rdf_term
                               % ?List:or([bnode,iri])
                               % ?Graph:atom
    rdf_list_directly_before/4, % ?Before:rdf_term
                                % ?After:rdf_term
                                % ?List:or([bnode,iri])
                                % ?Graph:atom
    rdf_list_last/2, % ?List:or([bnode,iri])
                     % ?Last:rdf_term
    rdf_list_last/3, % ?List:or([bnode,iri])
                     % ?Last:rdf_term
                     % ?Graph:atom
    rdf_list_nth0/4 % ?Index:nonneg
                    % ?List:or([bnode,iri])
                    % ?Element:rdf_term
                    % ?Graph:atom
  ]
).

%! rdf_list_after(?After:rdf_term, ?Before:rdf_term) is nondet.

rdf_list_after(After, Before):-
  rdf_list_after(After, Before, _, _).

%! rdf_list_after(
%!   ?After:rdf_term,
%!   ?Before:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds in case After occurs after Before in an RDF list.
%
% @see The inverse of rdf_list_before/4.

rdf_list_after(After, Before, List, Graph):-
  rdf_list_before(Before, After, List, Graph).



%! rdf_list_before(?Before:rdf_term, ?After:rdf_term) is nondet.

rdf_list_before(Before, After):-
  rdf_list_before(Before, After, _, _).

%! rdf_list_before(
%!   ?Before:rdf_term,
%!   ?After:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds in case Before occurs before After in an RDF list.
%
% @see Transitive closure of rdf_list_directly_before/3.

rdf_list_before(Before, After, List, Graph):-
  closure(
    \Before^After^rdf_list_directly_before(Before, After, List, Graph),
    Before,
    After
  ).



%! rdf_list_directly_after(
%!   ?After:rdf_term,
%!   ?Before:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds in case After occurs directly after Before in an RDF list.
%
% @see Inversion of rdf_list_directly_before/4.

rdf_list_directly_after(After, Before, List, Graph):-
  rdf_list_directly_before(Before, After, List, Graph).



%! rdf_list_directly_before(
%!   ?Before:rdf_term,
%!   ?After:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds in case Before occurs directly before After in an RDF list.

rdf_list_directly_before(Before, After, List, Graph):-
  rdf(List, rdf:first, Before, Graph),
  rdf(List, rdf:rest, Rest, Graph),
  rdf(Rest, rdf:first, After, Graph).



%! rdf_list_last(?List:or([bnode,iri]), ?Last:rdf_term) is det.

rdf_list_last(List, Last):-
  rdf_list_last(List, Last, _).

%! rdf_list_last(
%!   ?List:or([bnode,iri]),
%!   ?Last:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Pairs of lists and their last element.

rdf_list_last(List, Last, Graph):-
  rdf(List, rdf:rest, rdf:nil, Graph), !,
  rdf(List, rdf:first, Last, Graph).
rdf_list_last(List, Last, Graph):-
  rdf(List, rdf:rest, Rest, Graph),
  rdf_list_last(Rest, Last, Graph).



%! rdf_list_length(?List:or([bnode,iri]), ?Length:nonneg) is nondet.

rdf_list_length(List, N):-
  rdf_list_length(List, N, _).



%! rdf_list_length(
%!   ?List:or([bnode,iri]),
%!   ?Length:nonneg,
%!   ?Graph:atom
%! ) is nondet.
% Succeeds if Length is the number of elements in List.

rdf_list_length(List, N, Graph):-
  rdf_list_length(List, 0, N, Graph).

rdf_list_length(rdf:nil, N, N, _):- !.
rdf_list_length(List, N1, N3, Graph):-
  rdf(List, rdf:rest, Rest, Graph),
  rdf_list_length(Rest, N1, N2, Graph),
  succ(N2, N3).



%! rdf_list_nth0(
%!   ?Index:nonneg,
%!   ?List:rdf_term,
%!   ?Element:rdf_term,
%!   ?Graph:rdf_graph
%! ) is det.

rdf_list_nth0(I, L, E, G):-
  rdf_list_nth0(0, I, L, E, G).

rdf_list_nth0(I, I, L, E, G):-
  rdf_list_first(L, E, G).
rdf_list_nth0(I1, I3, L, E, G):-
  rdf(L, rdf:rest, R, G),
  rdf_list_nth0(I1, I2, R, E, G),
  succ(I2, I3).
