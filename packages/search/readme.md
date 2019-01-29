<h1 align="center">ts-earch-search</h1>

<p align="center">
Search algorithms and utilities for <code>ts-earch</code>
</p>

## Query DSL

`ts-earch` allows to search for TypeScript functions and methods by a query
language that is a subset of TypeScript's type declarations with some
differences.

### What is supported?

**Types**:

```
string              => string

User                => string

{ name: string }    => string

string | number     => string

string[]            => string

Array<string>       => string

Map<string, number> => string

(error? => void)    => void
```

**Multiple parameters**:

Separate types by comma, no need for parens.

```
string[], number, boolean => string
```

**Optional parameters**:

Just append a question mark after the type, `<type>?`.

```
string?           => string

User?             => string

{ name: string }? => string

string | number?  => string

string[]?         => string
```

**Functions returning functions**:

```
string          => string         => string

string, boolean => string, number => string
```

**Generics**

```
<A> A       => string

<A, B> A    => B

<A, B, C> A => B     => C

<A> A       => <B> B => string
```

**Methods**

```
SomeClass    :: ()         => string

SomeClass    :: string     => string

SomeClass<A> :: A          => string

SomeClass<A> :: <B> A      => B
```

### Examples

`compose`:

```
<A, B, C> (B => C), (A => B) => A => C
```

`R.map`

```
<A, B> (A => B) => A[] => B[]
```

`lodash.map`

```
<A, B> A[], (A => B) => B[]
```

`String.replace`:

```
String :: RegExp | string, string => string;
```

`Array.map`:

```
Array<T> :: <U> (T, number, T[] => U), any => U[]
```
