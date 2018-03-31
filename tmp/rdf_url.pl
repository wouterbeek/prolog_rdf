    is_rdf_url/1 % +Uri

:- use_module(library(lists)).

%! is_rdf_url(+Uri:atom) is semidet.

is_rdf_url(Uri) :-
  uri_file_extensions(Uri, Exts),
  member(Ext, Exts),
  media_type_extension(MediaType, Ext),
  rdf_media_type(MediaType), !.
