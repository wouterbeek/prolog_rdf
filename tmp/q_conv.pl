q_conv_void(Alias, Opts) :-
  rdf_global_id(Alias:void, VoidG),
  (   q_exists(VoidG)
  ->  debug(q(conv), "Graph ~a already exists in store.", [VoidG])
  ;   rdf_global_id(Alias:data, DataG),
      atomic_list_concat([Alias,load,void], '_', Pred),
      Goal_3 = Opts.module:Pred,
      q_load(hdt, DataG),
      source_to_void(DataG, Goal_3, VoidG),
      q_unload(DataG),
      debug(q(conv), "Graph ~a converted to store.", [VoidG])
  ).
