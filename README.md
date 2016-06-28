plRdf
=====

Additional support for RDF 1.1 support for SWI-Prolog.



Installation
------------

  1. Install [SWI-Prolog](http://www.swi-prolog.org/Download.html).
  
  2. Run the following from the SWI-Prolog top-level:
  
     ```prolog
     ?- pack_install(plRdf).
     ```


Disk or memory?
---------------

  * Disk-based SGP access: `hdt/4`, `hdt_<NAME>/<ARITY>`.
  
  * Memory-based SGP access: `rdf/[3,4]`, `rdf_<NAME>/<ARITY>`.
  
  * Shared private implementation: `rdf_<NAME>0/<ARITY+1>`, where:
  
    * `hdt_<NAME>(<ARTIY-ARGS>) :- rdf_<NAME>0(disk, <ARITY-ARGS>).`
    
    * `rdf_<NAME>(<ARTIY-ARGS>) :- rdf_<NAME>0(mem, <ARITY-ARGS>).`



Abbreviations
-------------

We use the following RDF-specific abbreviations for often occurring
variable names:

| **Variable name** | **Expansion**  |
|:-----------------:|:--------------:|
| `B`               | Blank node     |
| `C`               | Class          |
| `D`               | Datatype       |
| `G`               | Graph          |
| `I`               | Instance       |
| `Lit`             | Literal        |
| `Lex`             | Lexical form   |
| `Name`            | Name           |
| `Node`            | Node           |
| `NS`              | Namespace      |
| `O`               | Object term    |
| `P`               | Predicate term |
| `Prop`            | Property       |
| `Quad`            | Quadruple      |
| `Rel`             | Relation       |
| `S`               | Subject term   |
| `Term`            | Term           |
| `Triple`          | Triple         |
| `Tuple`           | Tuple          |
| `Val`             | Value          |
