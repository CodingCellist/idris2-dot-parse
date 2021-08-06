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
    - [x] types of identifier
    - [x] edge operators ('--' and '->')
    - [ ] (the other terminals?...)
  - [ ] "any amount of whitespace may be inserted between the terminals" (from
        the DOT language reference)
  - [x] identifiers
  - [x] assignment (_ID_ '=' _ID_)
  - [ ] handle string concatenation
  - [x] attributes
    - [x] `a_list`
    - [x] `attr_list`
    - [x] `attr_stmt`
  - [x] ports
  - [x] nodes
    - [x] `node_id`
    - [x] `node_stmt`
  - [x] edges
    - [x] `edgeop`
    - [x] `edgeRHS`
    - [x] `edge_stmt`
  - [x] subgraphs
  - [x] statements (`stmt`)
  - [x] list of statements (`stmt_list`)
  - [x] **graphs**, the thing at the top of it all
  - [ ] handle comments
- [ ] Semantic analysis
- [ ] Idris DOT representation/reasoning
- [ ] ... the rest of this todo-list ...

# LICENSE

This work is licensed under the BSD-3-Clause license.

