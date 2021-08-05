# idris2-dot-parse

An Idris2 lexer and parser for
[the DOT language](https://graphviz.org/doc/info/lang.html).

# TODO-list

- [x] Lexer
  - [x] whitespace
  - [x] comments
  - [x] symbols (brackets, braces, separators, and equals)
  - [x] keywords
  - [x] compass points
  - [x] identifiers (ish)
    - [x] names
    - [x] numerals
    - [x] double-quoted strings
    - [ ] HTML-strings (HTML-parsing is probably a library/package on its own)
  - [x] operators
    - [x] edgeops (graph+digraph)
    - [x] multilines
    - [x] string concatenation
- [ ] Parser + AST (these are linked; AST-types get implemented as parser grows)
  - [ ] terminals
    - [x] symbols
    - [x] keywords
    - [x] compass points
    - [ ] (the other terminals?...)
  - [x] identifiers
  - [x] assignment (_ID_ '=' _ID_)
  - [x] attributes
    - [x] `a_list`
    - [x] `attr_list`
    - [x] `attr_stmt`
  - [x] ports
  - [x] nodes
    - [x] `node_id`
    - [x] `node_stmt`
  - [ ] edges
    - [ ] `edgeRHS`
    - [ ] `edge_stmt`
  - [ ] subgraphs
- [ ] Semantic analysis
- [ ] Idris DOT representation/reasoning
- [ ] ... the rest of this todo-list ...

# LICENSE

This work is licensed under the BSD-3-Clause license.

