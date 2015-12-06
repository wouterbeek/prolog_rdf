%%  rdfs_individual_of2(+Resource, +Class) is semidet.
%%  rdfs_individual_of2(+Resource, -Class) is nondet.
%%  rdfs_individual_of2(-Resource, +Class) is nondet.
%
%  Generate resources belonging to a class   or  classes a resource
%  belongs to. We assume everything at the `object' end of a triple
%  is a class. A validator should confirm this property.
%
%  rdfs_individual_of(+, -) does  not  exploit   domain  and  range
%  properties, deriving that if rdf(R,  P,   _)  is  present R must
%  satisfy the domain of P (and similar for range).
%
%  There are a few hacks:
%
%    * Any resource is an individual of rdfs:Resource
%    * literal(_) is an individual of rdfs:Literal

rdfs_individual_of2(Resource, Class) :-
  nonvar(Resource), !,
  (   nonvar(Class)
  ->  (   rdf_equal(Class, rdfs:'Resource')
      ->  true
      ;  rdfs:rdfs_individual_of_r_c(Resource, Class)
      ->  true
      )
  ;   rdfs:rdfs_individual_of_r_c(Resource, Class)
  ).
rdfs_individual_of2(Resource, Class) :-
  nonvar(Class), !,
  (   rdf_equal(Class, rdfs:'Resource')
  ->  rdf_subject(Resource)
  ;   rdf_equal(Class, rdfs:'Class')
  ->  (   rdf_has(_, rdf:type, Resource)
      ;   rdf_has(Resource, rdfs:subClassOf, _)
      ;   rdf_has(_, rdfs:subClassOf, Resource)
      ;   rdf_has(_, rdfs:domain, Resource)
      ;   rdf_has(_, rdfs:range, Resource)
      ;   rdfs_subclass_of(SubClass, rdfs:'Class'),
          rdf_has(Resource, rdf:type, SubClass)
      ;   rdfs_subclass_of(SubClass, rdfs:'Datatype'),
          rdf_has(Resource, rdf:type, SubClass)
      )
  ;   rdfs_subclass_of(SubClass, Class),
      rdf_has(Resource, rdf:type, SubClass)
  ).
rdfs_individual_of2(_Resource, _Class) :-
  instantiation_error(_).
