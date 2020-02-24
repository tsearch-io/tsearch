<p align="center">
<img alg="tsearch logo" src="./assets/tsearchio.png" width="200" />
</p>

<h1 align="center">tsearch</h1>
<p align="center">Search engine for TypeScript functions, methods and types.</p>

## devlog

- [A crazy idea and a proof of concept](https://dev.to/gillchristian/a-crazy-idea-and-a-proof-of-concept-2oj7).
- [tsearch first public version](https://dev.to/gillchristian/tsearch-first-public-version-50a).

## Architecture

`tsearch` consists of 3 differt applications, found in the
[tsearch-io](https://github.com/tsearch-io/tsearch) organization:

- [indexer](https://github.com/tsearch-io/tsearch/indexer): for extracting type
  information from TypeScript to generate an index.
- [server](https://github.com/tsearch-io/tsearch/server): A server application
  to search the index.
- [client](https://github.com/tsearch-io/tsearch/client): The UI to search and
  view results.

## Development

For more information on how to run and develop Tsearch check the three repos
mentioned above.

Be aware that Tsearch is under development and the documentation might not be up
to date.

## Inspiration

`tsearch` is inspired on Haskell API search engine
[Hoogle](https://www.haskell.org/hoogle/).

## LICENSE

[MIT License](https://github.com/tsearch-io/tsearch/blob/master/LICENSE) ©
[Christian Gill](https://gillchristian.xyz)
