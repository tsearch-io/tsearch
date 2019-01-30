<h1 align="center">:mag: ts-earch :mag:</h1>

<p align="center">
:warning: <strong>WARNING!!! WORK IN PROGRESS</strong> :construction:
</p>

`ts-earch` is a search engine for TypeScript functions, methods and types.

[Demo](https://www.youtube.com/watch?v=Gf5irOLIFX8)

<a target="_blank" href="https://www.youtube.com/watch?v=Gf5irOLIFX8">
  <img
    src="https://media.giphy.com/media/8c76egNn04cNSilvd8/giphy.gif"
    alt="ts-earch demo" />
</a>

## Architecture

`ts-earch` consists of 3 differt applications, found in the `packages/`
directory:

- The `cli` for extracting function types from TypeScript projects.
- The `server` that performs searching on the types extracted by the `cli`.
- The `client` UI to search for types.

## Development

`ts-earch` is still under development, for now to run it you need to clone the
repo:

```bash
git clone https://github.com/gillchristian/ts-earch.git # https
git clone git@github.com:gillchristian/ts-earch.git     # ssh
```

Install all dependencies ([lerna](https://lernajs.io/) takes care of installing
the dependencies of the packages):

```bash
$ yarn
```

Extract function types to search with the client. This step is **required**
otherwise there will be no types to search:

```bash
$ yarn extract '/absolute/path/to/typescript/project/**/*.{ts,tsx}'
```

### Running

To use `ts-earch` we have to build first and the run the app. After that you can
open it in [localhost:3000](http://localhost:3000):

```bash
$ yarn build
$ yarn start
```

### Development

For development we run the watcher and the dev server in a different terminal
process/tab:

```bash
$ yarn watch
```

```bash
$ yarn dev
```

The server runs in [localhost:8080](http://localhost:8080) and the client in
[localhost:3000](http://localhost:3000).

## Inspiration

`ts-earch` is inspired on Haskell API search engine
[Hoogle](https://www.haskell.org/hoogle/).

## LICENSE

[MIT License](https://github.com/gillchristian/ts-earch/blob/master/LICENSE) ©
[Christian Gill](https://gillchristian.xyz)
