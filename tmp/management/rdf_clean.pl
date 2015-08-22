rdf_clean_stream(In, M, Dir):-
  create_bases(BaseUri, BNodeBase),
  set_stream(In, file_name(BaseUri)),
  rdf_guess_format(In, M, InFormat),
  
create_bases(BaseUri, http-'lodlaundromat.org'-Uuid):-
  uuid(Uuid0),
  atomic_list_concat(UuidComps, -, Uuid0),
  atomic_list_concat(UuidComps, '', Uuid),
  atomic_list_concat(['',Uuid], /, Path),
  uri_components(BaseUri, uri_components(http,'lodlaundromat.org',Path,_,_)).

rdf_clean_triples(In, BaseUri, InFormat, CleaningFile, BNodeBase):-
  InOpts = [
    base_uri(BaseUri),
    format(InFormat),
    graph(user),
    max_errors(-1),
    register_namespaces(false),
    silent(true),
    syntax(style)
  ],
  OutOpts = [bnode_base(BNodeBase)],
  rdf_clean_triples0(InFormat, In, InOpts, CleaningFile, OutOpts).
