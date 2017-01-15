:- module(
  musicbrainz,
  [
    mb_file/2,       % -File, -LMod
    mb_latest_file/1 % -File
  ]
).

/** <module> MusicBrainz

@author Wouter Beek
@version 2014/09-2014/10, 2016/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(thread)).
:- use_module(library(uri)).
:- use_module(library(xpath)).





%! mb_date(-Year, -Month, -Day) // is det.

mb_date(YYYY, MM, DD) -->
  #(4, digit, YYYYs),
  {pos_sum(YYYYs, YYYY)},
  #(2, digit, MMs),
  {pos_sum(MMs, MM)},
  #(2, digit, DDs),
  {pos_sum(DDs, DD)}, !.



%! mb_dir(-Dir, -LMod) is nondet.

mb_dir(Dir, date(YYYY,MM,DD)) :-
  Iri = 'http://linkedbrainz.org/rdf/dumps/',
  xml_download(Iri, Dom),
  xpath(Dom, //tr/td/a(@href), Href),
  atom_phrase(mb_date(YYYY,MM,DD), Href, _Rest),
  uri_normalized(Href, Iri, Dir).



%! mb_file(-File, -LMod) is nondet.

mb_file(File, LMod) :-
  mb_dir(Dir, LMod),
  xml_download(Dir, Dom),
  xpath(Dom, //a(@href), Href),
  file_name_extension(_, gz, Href),
  uri_normalized(Href, Dir, File).



%! mb_latest_file(-File) is det.
% Latest directory IRI.

mb_latest_file(File) :-
  findall(LMod-File0, mb_file(File0, LMod), Pairs),
  desc_pairs_values(Pairs, [File|_]).
