%! t(+Backend, ?S, ?P, ?O) is nondet.

t(rdf, S, P, O1) :-
  rdf11:pre_object(O1, O2),
  rdf(S, P, O2),
  rdf11:post_object(O1, O2).
t(rdf(G), S, P, O1) :-
  rdf11:pre_object(O1, O2),
  rdf(S, P, O2, G),
  rdf11:post_object(O1, O2).
%t(hdt(HdtOrG), S, P, O) :-
%  (hdt_graph(Hdt, HdtOrG) -> true ; Hdt = HdtOrG),
%  hdt_triple(Hdt, S, P, O).
