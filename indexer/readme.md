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
./bin/tsearch-index /path/to/typescript/project

# write to a file
./bin/tsearch-index /path/to/typescript/project --out index.json
```

To **run on multiple projects**:

It is possible to run the indexer in parallel on all the projects of a
directory.

```bash
./bin/tsearch-index-parallel /path/to/typescript/directory/* > index.json
```

The script requires [GNU parallel](https://www.gnu.org/software/parallel/) and
[jq](https://stedolan.github.io/jq/).

## LICENSE

[MIT License](https://github.com/tsearch-io/indexer/blob/master/LICENSE) ©
[Christian Gill](https://gillchristian.xyz)
