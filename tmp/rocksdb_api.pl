%! rocksdb_get(+Db:atom, +Key:atom, -Value:atom) is nondet.

rocksdb_get(Db, Key, Val):-
  run_process(
    rocksdb,
    [Db,get,Key],
    [output_goal(\Read^read_line_to_codes(Read,Cs)),program('RocksDB GET')]
  ),
  phrase(vals(Vals), Cs),
  % NONDET
  member(Val, Vals).

vals(L) --> dcg_bracketed(square, vals0(L)).
vals0([H|T]) --> dcg_atom(val0, H), !, vals0(T).
vals0([]) --> "".
val0(Cs) --> ...(Cs), " ", !.
val0(Cs) --> ...(Cs), eos.



%! rocksdb_put(+Db:atom, +Key:atom, +Value:atom) is det.

rocksdb_put(Db, Key, Val):-
  run_process(rocksdb, [Db,put,Key,Val], [program('RocksDB PUT')]).
