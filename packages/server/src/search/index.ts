import { Param, FunctionRecord } from '../models/tSearch'
import { compose } from '../utils'

interface SingleType {
  _type: 'single-type'
  value: string
}

interface FuncntionType {
  _type: 'function-type'
  parameters: string[]
  returnType: string
}

type Query = SingleType | FuncntionType

interface Matcher {
  func: (
    parameters: string[],
    returnType: string,
    fn: FunctionRecord,
  ) => boolean
  single: (value: string, fn: FunctionRecord) => boolean
}

const matchQuery = (m: Matcher) => (query: Query) => (
  fn: FunctionRecord,
): boolean => {
  if (query._type === 'single-type') {
    return m.single(query.value, fn)
  }

  return m.func(query.parameters, query.returnType, fn)
}

const matchParams = (query: string) => (param: Param) =>
  param.type.includes(query) || (param.name && param.name.includes(query))

const matchesSingleType = (query: string, fn: FunctionRecord) => {
  const paramMatcher = matchParams(query)

  const byName = fn.name && fn.name.includes(query)
  const byReturnType = fn.returnType.includes(query)
  const byParams = fn.parameters.filter(paramMatcher).length > 0

  return byName || byReturnType || byParams
}

const matchesFunctionType = (
  queryParams: string[],
  returnType: string,
  fn: FunctionRecord,
) =>
  fn.returnType === returnType &&
  fn.parameters
    .map(({ type }) => type)
    .some(param => queryParams.includes(param))

export const parseQuery = (query: string): Query => {
  const [params, returnType] = query.replace(/\s/g, '').split('=>')

  return Boolean(returnType)
    ? {
        _type: 'function-type',
        parameters: params.replace(/(\(|\))/g, '').split(','),
        returnType,
      }
    : {
        _type: 'single-type',
        value: query,
      }
}

export const matchesQuery = compose(
  matchQuery({
    single: matchesSingleType,
    func: matchesFunctionType,
  }),
  parseQuery,
)
