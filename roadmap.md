## Beta

- Extractor/Indexer
  - Extract all exported functions and arrow functions
  - Ungly types (i.e. not resolved imports)
  - One flat array (not real index)
  - Don't follow node_modules
- CLI
  - Indexing directory
  - Run server
- Server & search
  - Search from indexed directory
  - MVP Search UI

## v1

- Extractor/Indexer
  - Project compiler options 
  - Follow node_modules
  - Better types (i.e. resolve imports)
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
  - Search algorithm
