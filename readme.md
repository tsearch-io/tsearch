<h1 align="center">:mag: ts-earch :mag:</h1>

> **FULL DISCLOSURE**: this project is a **WORK IN PROGRESS**.

## Architecture

`ts-earch` consists of 3 differt applications, found in the `packages/`
directory:

- The `cli` for extracting function types from TypeScript projects.
- The `server` that performs searching on the types extracted by the `cli`.
- The `client` UI to search for types.

## Development

Install all dependencies ([lerna](https://lernajs.io/)) takes care of installing
the dependencies of the packages):

```bash
$ yarn
```

Extract function types to search with the client (this is required for the
server to work):

```bash
$ yarn extract '/absolute/path/to/typescript/project/**.{ts,tsx}'
```

Watch the code for changes and run the dev mode, server (`localhost:8080`) and
client (`localhost:3000`), each command in a different terminal process/tab:

```bash
$ yarn watch
```

```bash
$ yarn dev
```

Run in static mode (uses same ports):

```bash
$ yarn build
$ yarn start
```
