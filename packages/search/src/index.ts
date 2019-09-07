import { zip } from 'fp-ts/lib/Array'
import { fold, monoidSum } from 'fp-ts/lib/Monoid'

import { Type as TypeO, FunctionRecord } from 'ts-earch-types'

import { parseQuery, isName, FunctionType } from './parse'

const sum = fold(monoidSum)

const matchesNameType = (query: string) => (fn: FunctionRecord) =>
  Boolean(fn.name && fn.name.includes(query))

// should become a range when considering type checking
// e.g. weighReturnType('string', 'string')            => 2
//      weighReturnType('string', StringAlias)         => 1.x ?
//      weighReturnType('string', 'string' | 'number') => 1
// const weighReturnType = (query: string, returnType: string) =>
//   query === returnType ? 2 : 0

const weighParams = (query: TypeO[], params: TypeO[]) => {
  let paramsWeight = 0

  // m -> matches
  // n -> # of params
  if (query.length === params.length) {
    paramsWeight =
      sum(zip(query, params).map(([q, p]) => weighParam(q)(p))) / query.length

    // + 1 + m/n # only if some params match
    if (paramsWeight > 0) {
      paramsWeight += paramsWeight + 1
    }
  } else {
    // + m/n
    // TODO
    paramsWeight +=
      query.reduce((acc, type) => (params.includes(type) ? acc + 1 : acc), 0) /
      query.length
  }

  return paramsWeight
}

function weighParam(query: TypeO): (param: TypeO) => number {
  return TypeO.match({
    Any: () => 1,
    Unknown: () => 1,
    Undefined: () => (TypeO.isUndefined(query) ? 1 : 0),
    LiteralPrimitive: ({ text }) =>
      TypeO.isLiteralPrimitive(query) ? (query.text === text ? 1 : 0) : 0,
    Primitive: ({ typeName }) =>
      TypeO.isPrimitive(query) ? (query.typeName === typeName ? 1 : 0) : 0,
    Array: ({ elementsType }) =>
      TypeO.isArray(query)
        ? // TODO: `weighType` -> to be used for tuples, unions,
          // intersections, functions, etc
          weighParam(query.elementsType)(elementsType) === 1
          ? 1
          : // TODO: find proper weight when `elementsType` don't match
            0.5
        : 0,
    Union: () => 1,
    Intersection: () => 1,
    Tuple: ({ types }) =>
      TypeO.isTuple(query)
        ? query.types.length === types.length
          ? sum(zip(query.types, types).map(([q, p]) => weighParam(q)(p))) /
            types.length
          : 0
        : 0,
    Function: () => 0,
    HigherOrder: p => {
      if (TypeO.isHigherOrder(query)) {
        // TODO: type name (`text` has the whole definition)
        const typeMatch = p.text.split('<')[0] === query.text.split('<')[0]

        const args =
          sum(
            zip(query.arguments, p.arguments).map(([q, p]) => weighParam(q)(p)),
          ) / p.arguments.length

        return typeMatch ? (args === 1 ? 1 : 0.5) : 0
      }

      return 0
    },
    Other: ({ text }) =>
      TypeO.isOther(query) ? (text === query.text ? 1 : 0) : 0,
  })
}

export const weighFunctionRecord = ({ signature }: FunctionType) => (
  fn: FunctionRecord,
): [FunctionRecord, number] => {
  const { returnType, parameters } = signature.signatures[0]

  const weight =
    weighParams(
      fn.signature.parameters.map(({ type }) => type),
      parameters.map(({ type }) => type),
    ) +
    // TODO: `weighReturnType`
    weighParam(returnType)(fn.signature.returnType)

  return [fn, weight]
}

export const search = (types: FunctionRecord[]) => (query: string) => {
  const q = parseQuery(query)

  return isName(q)
    ? types.filter(matchesNameType(q.value)).slice(0, 100)
    : types
        .map(weighFunctionRecord(q))
        .filter(([, weight]) => weight > 0)
        .sort(([, a], [, b]) => (a < b ? 1 : -1))
        .slice(0, 100)
        .map(([fn]) => fn)
}
