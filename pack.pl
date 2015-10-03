:- dynamic(author/2).
:- dynamic(download/1).
:- dynamic(home/1).
:- dynamic(maintainer/2).
:- dynamic(name/1).
:- dynamic(packager/2).
:- dynamic(requires/1).
:- dynamic(title/1).
:- dynamic(version/1).

author('Wouter Beek', 'me@wouterbeek.com').
download('https://github.com/wouterbeek/plRdf/release/*.zip').
home('https://github.com/wouterbeek/plRdf').
maintainer('Wouter Beek', 'me@wouterbeek.com').
name(plRdf).
packager('Wouter Beek', 'me@wouterbeek.com').
requires(sparkle).
requires(plXsd).
title('Extended support for handling RDF data in SWI-Prolog.').
version('0.1.0').
