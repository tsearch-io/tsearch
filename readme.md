<h1 align="center">:mag: ts-earch :mag:</h1>

## Development

`ts-earch` consists of 3 differt apps, found in the `packages/` directory:

- `packages/cli/`: a CLI application to extract function types from TypeScript projects.
- `packages/server/`: the server that performs searching on the types extracted by the
  CLI.
- `packages/app/`: the UI to search for types.

To work with all packages we use [lerna](https://lernajs.io/):

Install all dependencies:

```bash
$ yarn
$ yarn lerna exec yarn
```

Run TypeScript in watch mode, for `packages/app` it runs the dev server (`localhost:3000`):

```bash
$ yarn lerna run watch
```

Run `packages/cli` to extract function types:

```bash
$ cd packages/cli
$ node build/cli '/absolute/path/to/typescript/project/**.{ts,tsx}'
```

Run `packages/server` (`localhost:8080`):

```bash
$ cd packages/server
$ yarn dev
```
