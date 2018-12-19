## Beta

- Extractor/Indexer
  - Extract all exported functions and arrow functions
  - Ungly types (i.e. not resolved imports)
  - One flat array (not real index)
  - Don't follow `node_modules`
- CLI
  - Indexing directory
- Server & search
  - Search from indexed directory
  - MVP Search UI
  - Simple search algorithm

## v1

- Extractor/Indexer
  - Use directory's (or project) compiler options
  - Follow `node_modules`
  - Better types
    - Resolve imports
    - Include docs (or comments)
- Server & Search
  - Improved UI
    - Highlight matches
    - Open file in `$EDITOR`

## v2

- Extractor/Indexer
  - Prototype indexing npm repository
- Server & Search
  - Improved Search algorithm by TS semantics
