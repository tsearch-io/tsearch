import { Param, FunctionRecord } from '../models/tSearch'

const matchParams = (query: string) => (param: Param) =>
  param.type.includes(query) || (param.name && param.name.includes(query))

export const searchFunctions = (query: string) => (fn: FunctionRecord) => {
  const paramMatcher = matchParams(query)

  const byName = fn.name && fn.name.includes(query)
  const byReturnType = fn.returnType.includes(query)
  const byParams = fn.parameters.filter(paramMatcher).length > 0

  return byName || byReturnType || byParams
}

