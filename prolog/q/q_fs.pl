:- module(
  q_fs,
  [
    q_delete_file/1,      % +File  
    q_dir/1,              % ?Dir
    q_dir/2,              % +HashG, -Dir
    q_dir_file/4,         % ?Dir, ?Name, ?Format, ?File
    q_dir_graph/2,        % ?Dir, ?G
    q_dir_graph/3,        % ?Dir, ?Name, ?G
    q_dir_hash/2,         % ?Dir, ?Hash
    q_file/3,             % ?Name, ?Format, ?File
    q_file_graph/2,       % ?File, ?G
    q_file_graph/3,       % ?File, ?Format, ?G
    q_file_hash/2,        % ?File, ?Hash
    q_file_hash/4,        % ?File, ?Name, ?Format, ?Hash
    q_file_is_ready/1,    % +File
    q_file_is_ready/2,    % +File1, File2
    q_file_ready/2,       % +File, -Ready
    q_file_ready_time/2,  % +File, -Time
    q_file_touch_ready/1, % +File
    q_format/1,           % ?Format
    q_format/2,           % ?Format, -Exts
    q_graph/2,            % +Name, ?G
    q_graph/3,            % +HashG, +Name, ?G
    q_graph_hash/2,       % ?G, ?Hash
    q_graph_hash/3,       % ?G, ?Name, ?Hash
    q_hash/1,             % -Hash
    q_name/1              % ?Name
  ]
).

/** <module> Quine file system

@author Wouter Beek
@version 2016/08-2016/09
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).
:- use_module(library(q/q_io), []).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).





%! q_delete_file(+File) is det.

q_delete_file(File) :-
  delete_file_msg(File),
  q_delete_ready(File).



%! q_delete_ready(+File) is det.

q_delete_ready(File) :-
  q_file_ready(File, Ready),
  delete_file_msg(Ready).



%! q_dir(+Dir) is semidet.
%! q_dir(-Dir) is nondet.
%
% Directory of a data graph.

q_dir(Dir3) :-
  setting(q_io:store_dir, Dir1),
  directory_path(Dir1, Dir2),
  directory_path(Dir2, Dir3).



%! q_dir(+HashG, -Dir) is nondet.
%
% Directories with the given HashG.
%
% Identical to q_dir/1 with the empty prefix.

q_dir('', Dir) :- !,
  q_dir(Dir).
q_dir(HashG, Dir2) :-
  atom_length(HashG, N),
  setting(q_io:store_dir, Root),
  (   % HashG falls within the first two characters (outer
      % directory).
      N =< 2
  ->  atom_concat(HashG, *, Wildcard0),
      append_directories(Root, Wildcard0, Wildcard),
      expand_file_name(Wildcard, Dirs),
      member(Dir1, Dirs),
      directory_path(Dir1, Dir2)
  ;   % HashG goes past the first two characters (inner
      % directory).
      atom_codes(HashG, [H1,H2|T1]),
      atom_codes(Dir1, [H1,H2]),
      append(T1, [0'*], T2),
      atom_codes(Wildcard0, T2),
      append_directories([Root,Dir1,Wildcard0], Wildcard),
      expand_file_name(Wildcard, Dirs2),
      member(Dir2, Dirs2)
  ).



%! q_dir_file(+Dir, +Name, +Format, -File) is det.
%! q_dir_file(+Dir, +Name, -Format, -File) is multi.
%! q_dir_file(+Dir, -Name, +Format, -File) is multi.
%! q_dir_file(+Dir, -Name, -Format, -File) is multi.
%! q_dir_file(-Dir, -Name, -Format, +File) is det.

q_dir_file(Dir, Name, Format, File) :-
  ground(File), !,
  directory_file_path(Dir, Local, File),
  atomic_list_concat([Name|Exts], ., Local),
  q_format(Format, Exts).
q_dir_file(Dir, Name, Format, File) :-
  q_name(Name),
  (   nonvar(Format)
  ->  once(q_format(Format, Exts))
  ;   q_format(Format, Exts)
  ),
  atomic_list_concat([Name|Exts], ., Local),
  directory_file_path(Dir, Local, File).



%! q_dir_graph(+Dir, -G) is multi.
%! q_dir_graph(-Dir, +G) is det.
%! q_dir_graph(+Dir, +Name, -G) is det.
%! q_dir_graph(+Dir, -Name, -G) is multi.
%! q_dir_graph(-Dir, -Name, +G) is det.

q_dir_graph(Dir, G) :-
  q_dir_graph(Dir, _, G).


q_dir_graph(Dir, Name, G) :-
  ground(Dir), !,
  q_dir_hash(Dir, Hash),
  q_graph_hash(G, Name, Hash).
q_dir_graph(Dir, Name, G) :-
  q_graph_hash(G, Name, Hash),
  q_dir_hash(Dir, Hash).



%! q_dir_hash(+Dir, -Hash) is det.
%! q_dir_hash(-Dir, +Hash) is det.

q_dir_hash(Dir, Hash) :-
  ground(Dir), !,
  directory_subdirectories(Dir, Subdirs),
  reverse(Subdirs, [Postfix,Prefix|_]),
  atom_concat(Prefix, Postfix, Hash).
q_dir_hash(Dir4, Hash) :-
  atom_codes(Hash, [H1,H2|T]),
  maplist(atom_codes, [Dir1,Dir2], [[H1,H2],T]),
  append_directories(Dir1, Dir2, Dir3),
  setting(q_io:store_dir, Dir0),
  directory_file_path(Dir0, Dir3, Dir4).



%! q_file(?Name, ?Format, -File) is nondet.
%! q_file(-Name, -Format, +File) is nondet.

q_file(Name, Format, File) :-
  ground(File), !,
  q_dir_file(Dir, Name, Format, File),
  q_dir(Dir).
q_file(Name, Format, File) :-
  q_dir(Dir),
  q_dir_file(Dir, Name, Format, File),
  exists_file(File).



%! q_file_graph(+File, -G) is det.
%! q_file_graph(-File, +G) is multi.
%! q_file_graph(+File, -Format, -G) is det.
%! q_file_graph(-File, -Format, +G) is multi.
%! q_file_graph(-File, +Format, +G) is det.

q_file_graph(File, G) :-
  q_file_graph(File, _, G).


q_file_graph(File, Format, G) :-
  ground(File), !,
  q_file_hash(File, Name, Format, Hash),
  q_graph_hash(G, Name, Hash).
q_file_graph(File, Format, G) :-
  q_graph_hash(G, Name, Hash),
  q_file_hash(File, Name, Format, Hash).



%! q_file_hash(+File, -Hash) is det.
%! q_file_hash(-File, +Hash) is multi.
%! q_file_hash(+File, -Name, -Format, -Hash) is det.
%! q_file_hash(-File, +Name, +Format, +Hash) is det.
%! q_file_hash(-File, +Name, -Format, +Hash) is multi.
%! q_file_hash(-File, -Name, +Format, +Hash) is multi.
%! q_file_hash(-File, -Name, -Format, +Hash) is multi.

q_file_hash(File, Hash) :-
  q_file_hash(File, _, _, Hash).


q_file_hash(File, Name, Format, Hash) :-
  ground(File), !,
  q_dir_file(Dir, Name, Format, File),
  q_dir_hash(Dir, Hash).
q_file_hash(File, Name, Format, Hash) :-
  q_dir_hash(Dir, Hash),
  q_dir_file(Dir, Name, Format, File).



%! q_file_is_ready(+File) is semidet.
%! q_file_is_ready(+File1, +File2) is semidet.

q_file_is_ready(File) :-
  q_file_ready(File, Ready),
  exists_file(Ready).


q_file_is_ready(File1, File2) :-
  q_file_ready_time(File2, Ready2),
  q_file_ready_time(File1, Ready1),
  Ready2 >= Ready1.



%! q_file_ready(+File, -Ready) is det.

q_file_ready(File, Ready) :-
  atomic_list_concat([File,ready], ., Ready).



%! q_file_ready_time(+File, -Time) is det.

q_file_ready_time(File, Time) :-
  q_file_ready(File, Ready),
  exists_file(Ready),
  time_file(Ready, Time).



%! q_file_touch_ready(+File) is det.

q_file_touch_ready(File) :-
  q_file_ready(File, Ready),
  touch(Ready).



%! q_format(+Format) is semidet.
%! q_format(-Format) is multi.
%! q_format(+Format, -Exts) is det.
%! q_format(-Format, -Exts) is multi.

q_format(Format) :-
  q_format(Format, _).


q_format(Format, Exts) :-
  q_io:q_store_format(Format, Exts).
q_format(Format, Exts) :-
  q_io:q_cache_format(Format, Exts).



%! q_graph(+Name, -G) is nondet.
%! q_graph(-Name, +G) is nondet.
%! q_graph(+HashG, +Name, -G) is nondet.
%! q_graph(-Hash, -Name, +G) is nondet.

q_graph(Name, G) :-
  ground(G), !,
  q_dir_graph(Dir, Name, G),
  q_dir(_, Dir).
q_graph(Name, G) :-
  q_hash(Hash),
  q_graph(Hash, Name, G).


q_graph(HashG, Name, G) :-
  ground(G), !,
  q_dir_graph(Dir, Name, G),
  q_dir(HashG, Dir).
q_graph(HashG, Name, G) :-
  q_dir(HashG, Dir),
  q_dir_graph(Dir, Name, G).



%! q_graph_hash(+G, -Hash) is det.
%! q_graph_hash(-G, +Hash) is multi.
%! q_graph_hash(+G, -Name, -Hash) is det.
%! q_graph_hash(-G, +Name, +Hash) is det.
%! q_graph_hash(-G, -Name, +Hash) is multi.

q_graph_hash(G, Hash) :-
  q_graph_hash(G, _, Hash).


q_graph_hash(G, Name, Hash) :-
  nonvar(G), !,
  rdf_global_id(Name:Hash, G).
q_graph_hash(G, Name, Hash) :-
  q_name(Name),
  rdf_global_id(Name:Hash, G).



%! q_hash(-Hash) is nondet.

q_hash(Hash) :-
  q_dir(Dir),
  q_dir_hash(Dir, Hash).



%! q_name(+Name) is semidet.
%! q_name(-Name) is multi.

q_name(data).
q_name(meta).
q_name(stat).
q_name(vocab).
q_name(void).
q_name(warn).
