## Beta

- [x] Extractor/Indexer
  - [x] Extract all exported functions and arrow functions
  - [x] Ungly types (i.e. not resolved imports)
  - [x] One flat array (not real index)
  - [x] Don't follow `node_modules`
- [x] CLI
  - [x] Indexing directory
- [x] Server & search
  - [x] Search from indexed directory
  - [x] MVP Search UI
  - [x] Simple search algorithm

## v1

- [ ] Extractor/Indexer
  - [ ] Use directory's (or project) compiler options
  - [ ] Follow `node_modules`
  - [ ] Better types
    - [ ] Resolve imports
    - [ ] Include docs (and/or comments)
- [ ] Server & Search
  - [ ] Improved UI
    - [ ] Highlight matches
    - [ ] Open file in `$EDITOR`

## v2

- [ ] Extractor/Indexer
  - [ ] Prototype indexing npm repository
- [ ] Server & Search
  - [ ] Improved Search algorithm by TS semantics
