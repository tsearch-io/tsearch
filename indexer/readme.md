# tsearch indexer

## :warning: Work in Progress :warning:

Generates the "index" of functions in a TypeScript project(s) used by the
[tsearch-io/server](https://github.com/tsearch-io/server).

### How to?

**Tsearch is under development, things might not work as expected.**

Clone the project.

Build with `yarn build`.

To **run on a single project**:

```bash
# output to the terminal
./bin/ts-earch /path/to/typescript/project

# write to a file
./bin/ts-earch /path/to/typescript/project --out out.json
```

To **run on multiple projects**:

The Go program (`main.go`) runs the indexer on all the projects in a directory.

```bash
# build first, no dependencies required
$ go build -o tsearch-index main.go

$ ./tsearch-index --help
Usage of ./tsearch-index:
  -bin string
        ts-earch bin path (default "./bin/ts-earch")
  -dt string
        Modules directory (default "~/dev/DefinitelyTyped/types")
  -out string
        out file path (default "~/.ts-earch")
  -x int
        only extract the first X modules (extracts all when 0)

# run on all the projects inside a path, output to current directory
$ ./tsearch-index -out ./ -dt ~/path/to/projects/directory
```

The output file will have name as date when written (eg.
`2020-01-30-06-17-05.json`), it can be used as the `modules.json` on
[tsearch-io/server](https://github.com/tsearch-io/server).

## LICENSE

[MIT License](https://github.com/tsearch-io/indexer/blob/master/LICENSE) ©
[Christian Gill](https://gillchristian.xyz)
