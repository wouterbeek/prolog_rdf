:- dynamic
    deref_thread_iri0/2.


start_deref_threads(Files) :-
  length(Files, N),
  numlist(1, N, Ns),
  maplist(start_deref_thread, Ns, Files).


start_deref_thread(N, File) :-
  atomic_list_concat([deref,N], '_', Alias),
  thread_create(run_deref_thread(File), _, [alias(Alias)]).


run_deref_thread(File) :-
  setup_call_cleanup(
    open(File, read, In),
    run_deref_thread_stream(In),
    close(In)
  ).


run_deref_thread_stream(In) :-
  get_next_iri(In, Iri),
  update_deref_thread_iri(Iri),
  deref_iri(Iri).


update_deref_thread_iri(Iri) :-
  thread_self(Alias),
  with_mutex(deref_thread_iri, (
    retractall(deref_thread_iri0(Alias, _)),
    assert(deref_thread_iri0(Alias, Iri))
  )).
