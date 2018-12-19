import { Param, FunctionRecord } from 'ts-earch-types'

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
export const isFunc = (q: Query): q is FunctionType =>
  q._type === Type.FUNCTION

const matchParams = (query: string) => (param: Param) =>
  param.type.includes(query) || (param.name && param.name.includes(query))

const matchesNameType = (query: string) => (fn: FunctionRecord) => {
  const paramMatcher = matchParams(query)

  const byName = fn.name && fn.name.includes(query)
  const byReturnType = fn.returnType.includes(query)
  const byParams = fn.parameters.filter(paramMatcher).length > 0

  return byName || byReturnType || byParams
}

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

const weighFunctionRecord = ({ returnType, parameters }: FunctionType) => (
  fn: FunctionRecord,
): [FunctionRecord, number] => {
  const fnParameters = fn.parameters.map(({ type }) => type)

  let paramsWeight =
    fnParameters.reduce(
      (acc, type, key) => (type === parameters[key] ? acc + 1 : acc),
      0,
    ) / fnParameters.length

  // m -> matches
  // n -> # of params
  if (fnParameters.length === parameters.length) {
    // + 1 + m/n
    paramsWeight += paramsWeight + 1
  } else {
    // + m/n
    paramsWeight +=
      fnParameters.reduce(
        (acc, type) => (parameters.includes(type) ? acc + 1 : acc),
        0,
      ) / fnParameters.length
  }

  let weight = paramsWeight + (returnType === fn.returnType ? 2 : 0)

  return [fn, weight]
}

export const search = (types: FunctionRecord[]) => (query: string) => {
  const q = parseQuery(query)

  if (isName(q)) {
    return types.filter(matchesNameType(q.value)).slice(0, 100)
  }

  return types
    .map(weighFunctionRecord(q))
    .filter(([, weight]) => weight > 0)
    .sort(([, a], [, b]) => (a < b ? 1 : -1))
    .slice(0, 100)
    .map(([fn]) => fn)
}
