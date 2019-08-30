import { FunctionRecord } from 'ts-earch-types'

const enum Type {
  NAME = 'name-type',
  FUNCTION = 'function-type',
}

interface NameType {
  _type: Type.NAME
  value: string
}

interface FunctionType {
  _type: Type.FUNCTION
  parameters: string[]
  returnType: string
}

type Query = NameType | FunctionType

export const isName = (q: Query): q is NameType => q._type === Type.NAME
export const isFunc = (q: Query): q is FunctionType => q._type === Type.FUNCTION

const matchesNameType = (query: string) => (fn: FunctionRecord) =>
  Boolean(fn.name && fn.name.includes(query))

export const parseQuery = (query: string): Query => {
  const [params, returnType] = query.replace(/\s/g, '').split('=>')

  return Boolean(returnType)
    ? {
        _type: Type.FUNCTION,
        parameters: params.replace(/(\(|\))/g, '').split(','),
        returnType,
      }
    : {
        _type: Type.NAME,
        value: query,
      }
}

// should become a range when considering type checking
// e.g. weighReturnType('string', 'string')            => 2
//      weighReturnType('string', StringAlias)         => 1.x ?
//      weighReturnType('string', 'string' | 'number') => 1
const weighReturnType = (query: string, returnType: string) =>
  query === returnType ? 2 : 0

const weighParams = (query: string[], params: string[]) => {
  let paramsWeight = 0

  // m -> matches
  // n -> # of params
  if (query.length === params.length) {
    paramsWeight =
      query.reduce(
        (acc, type, key) => (type === params[key] ? acc + 1 : acc),
        0,
      ) / query.length

    // + 1 + m/n # only if some params match
    if (paramsWeight > 0) {
      paramsWeight += paramsWeight + 1
    }
  } else {
    // + m/n
    paramsWeight +=
      query.reduce((acc, type) => (params.includes(type) ? acc + 1 : acc), 0) /
      query.length
  }

  return paramsWeight
}

export const weighFunctionRecord = ({
  returnType,
  parameters,
}: FunctionType) => (fn: FunctionRecord): [FunctionRecord, number] => {
  const fnParameters = fn.parameters.map(({ type }) => type)

  const weight =
    weighParams(fnParameters, parameters) +
    weighReturnType(returnType, fn.returnType)

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
