import { Alias, Union, Keyed, Enum, EnumWithDefaults, Interface } from './utils'

// --- generics

export const withGeneric = <T>(t: T) => t

export const withMultipleGeneric = <T, U>(t: T, u: U) => `${t}:${u}`

export const withMultipleRepeatedGeneric = <T, U>(t: T, u: U, us: U[]) =>
  us.map(_ => `${t}:${u}`)

export const withExtendsConstraint = <WithExtends extends string>(
  t: WithExtends,
) => t

export const withLiteralConstraint = <WithExtends extends { foo: 'string' }>(
  t: WithExtends,
) => t.foo

export const withDefaultTypeParam = <WithDefault = string>(t: WithDefault) => t

// --- higher order

interface HigherOrder<T> {
  t: T
}

export const withHigherOrder = (t: HigherOrder<string>) => t.t

interface Maybe<D, E> {
  data: D
  error: E
}

export const withHigherOrder2 = <D, E>(data: D, error: E): Maybe<D, E> => ({
  data,
  error,
})

// --- regular types

export const noGenerics = (t: string) => t

export const curried = (t: string) => (n: number) => `${t}:${n}`

export const inlineObjType = (t: { s: string }) => (n: number) => `${t.s}:${n}`

export function fnWithGenerics<T>(t: T) {
  return t
}

// --- using imports

export const withImported = (u: Union, a: Alias) => `${a}:${u}`

export const withEnum = (e: Enum) => e

export const withNumericEnum = (e: EnumWithDefaults) => e

// --- multiple exports

const exportA = withEnum
const exportB = withGeneric

export { exportA, exportB }

export function parseAll(
  headers: Record<string, string | string[] | undefined>,
): {
  charsets: string[]
  encodings: string[]
  languages: string[]
  mediaTypes: string[]
} {
  return { charsets: [], encodings: [], languages: [], mediaTypes: [] }
}
