# tsearch.io server

## :warning: Work in Progress :warning:

Server for [tsearch.io](https://tsearch.io).

### How to?

**Tsearch is under development, things might not work as expected.**

This is a [Stack](https://docs.haskellstack.org/en/stable/README/) application,
for instructions how to install dependencies and run check the
[Stack docs](https://docs.haskellstack.org/en/stable/GUIDE/).

Before running the application an index of types needs to be provided
(default `modules.json`). Check
[tsearch-io/indexer](https://github.com/tsearch-io/indexer) for instructions on
how to generate it.

If you are only looking to run Tsearch's server, a Docker image with an index is
provided:
[gillchristian/tsearch](https://hub.docker.com/repository/docker/gillchristian/tsearch).
At the moment the image only supports running with the provided index. In the
future it should support user provided index.

#### Server

To run the server:

```
stack run tsearch-sever
```

By default the server runs on port 8080 and looks for types index as
`./modules.json`.

```
PORT=9000 stack run tsearch-sever -- ./index.json
```

#### Search CLI

Tsearch can run from via the CLI.

```bash
stack run tsearch -- --index ./modules.json "A => A"
```

## LICENSE

[MIT License](https://github.com/tsearch-io/server/blob/master/LICENSE) ©
[Christian Gill](https://gillchristian.xyz)
