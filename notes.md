https://www.spock.li/tutorials/rest-api
https://haskell-at-work.com/episodes/2018-04-09-your-first-web-application-with-spock.html

---

**Aeson**

- Check the
  [Options](http://hackage.haskell.org/package/aeson-1.4.5.0/docs/Data-Aeson.html#t:Options)
  for better ways to handle the union cases and keys.
- Generate TypeScript defintions (Haskell should be source of truth) with
  [Data.Aeson.TypeScript](http://hackage.haskell.org/package/aeson-typescript-0.2.0.0/docs/Data-Aeson-TypeScript-TH.html).
- Use `(<?>)` to add better error messages on parsing

---

**CLI**: parallelization improvements

List all the files in a directory:

```
$ fd -d 1 '' ~/dev/DefinitelyTyped/types > files
```

Run ts-search CLI with [parallel](https://www.gnu.org/software/parallel/) and
write the output, one package per line, to `out`:

```
$ parallel ./bin/ts-earch < files > out
```

---

**Search**

- Filter by **arity** (if query has X parameter we only want functions with +/- 1
  parameter)
