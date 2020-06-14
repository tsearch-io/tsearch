# tsearchql

Query language for tsearch.io.

## Syntax for types

[**Basic types**](https://www.typescriptlang.org/docs/handbook/basic-types.html)

Literal matching

- `boolean`
- `string`
- `number`
- `object` (?)
- `undefined`
- `null`
- `void`: `null | undefined`
- `any`: matches anything (with lower weight if on compared side?)
- `never`
- `unkown`: similar to `any`

**Parametrized types**

Match type literally and list of types recursively (i.e. could also be
parametrized types).

- `Array<T>`
- `Promise<Data>`
- `Map<U, T>`
- `Array<string>`
- `Promise<Array<Data>>`

Special case `T[]` that converts to `Array<T>`.

**Generics**

- `T[] => T`: single capital leters are considered generics.
- `<Data> Data[] => Data`: can be specified

Get renamed to a canonical name and matched by using a type level lambda (System
F):

`λt. t[] => t`

**Union types**

Pipe (`|`) separated list of types: `number | string | boolean`

Split into a list of types and match all, order doesn't matter.

**Optional types**

Appended by a question mark: `number ?`

Sintax sugar for: `number | undefined` (`... | null`?).

**Functions**

Comma separated list of parameters, arrow (`=>`) and return type.

The arrow is right associative. Enclose in parens to avoid right association

Don't take generics (in the future they could).

Match is done by recursively matching parameters and return types.

**Tuples**

List of types enclosed in brackets: `[number, string]`.

Match list of types in order.

## Examples

```
number => number[]

RegExp, string => string => string

string => Promise<D>

(B => C), (A => B) => A => C

A[], B[] => [A, B][]

A[] => B[] => Array<[A, B]>

(A => B) , A[] => B[]

(A => B) => A[] => B[]

(A, number => B) => A[] => B[]

(A => void) => A[] => void
```

## What's missing?

- Literals: strings and numbers
- Interfaces (all object related types)
- Key-ed types (`{ [string]: T }`)
- Methods (`<object's-type> :: <function>`?)
- Enums
- Joins (`&`)
