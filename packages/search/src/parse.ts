import { Type as TypeO, FunctionT, TypeParameter } from 'ts-earch-types'

const enum Type {
  NAME = 'name-type',
  FUNCTION = 'function-type',
}

interface NameType {
  _type: Type.NAME
  value: string
}

export interface FunctionType {
  _type: Type.FUNCTION
  signature: FunctionT
}

export type Query = NameType | FunctionType

export const isName = (q: Query): q is NameType => q._type === Type.NAME
export const isFunc = (q: Query): q is FunctionType => q._type === Type.FUNCTION

const functionType = (
  query: string,
  params: string,
  returnType: string,
): FunctionType => {
  return {
    _type: Type.FUNCTION,
    signature: {
      __tag: 'Function',
      text: query,
      signatures: [
        {
          typeParameters: [] as TypeParameter[],
          parameters: params
            .replace(/(\(|\))/g, '')
            .split(',')
            .map(name => ({ name, type: nameToType(name) })),
          returnType: { __tag: 'Other', text: returnType },
        },
      ],
    },
  }
}

const genericRx = /^[A-Z]$/
const stringRx = /^'[^\t]*'$/
const arrayRx = /^.*\[\]$/
const tupleRx = /^\[.*\]$/
const higherOrderRx = /^[a-zA-Z]+<.*>$/

const nameToType = (name: string): TypeO => {
  if (name === 'any') {
    return { __tag: 'Any' }
  }

  if (name === 'unknown') {
    return { __tag: 'Unknown' }
  }

  if (name === 'undefined') {
    return { __tag: 'Undefined' }
  }

  if (name === 'string' || name === 'number' || name === 'boolean') {
    return { __tag: 'Primitive', text: name, typeName: name }
  }

  if (name === 'true' || name === 'false' || stringRx.test(name)) {
    return { __tag: 'LiteralPrimitive', text: name }
  }

  if (arrayRx.test(name)) {
    return {
      __tag: 'Array',
      text: name,
      elementsType: nameToType(name.replace(/\[\]$/, '')),
    }
  }

  if (name.includes('|')) {
    return {
      __tag: 'Union',
      text: name,
      types: name.split('|').map(n => nameToType(n.trim())),
    }
  }

  if (tupleRx.test(name)) {
    return {
      __tag: 'Tuple',
      text: name,
      types: name
        .replace(/^\[/, '')
        .replace(/\]$/, '')
        .split(',')
        .map(n => nameToType(n.trim())),
    }
  }

  if (higherOrderRx.test(name)) {
    return {
      __tag: 'HigherOrder',
      text: name,
      arguments: name
        .replace(/^[a-zA-Z]+</, '')
        .replace(/>$/, '')
        .split(',')
        .map(n => nameToType(n.trim())),
    }
  }

  // TODO: this should be handled differently
  if (genericRx.test(name)) {
    return { __tag: 'Other', text: name }
  }

  return { __tag: 'Other', text: name }
}

export const parseQuery = (query: string): Query => {
  const [params, returnType] = query.replace(/\s/g, '').split('=>')

  return Boolean(returnType)
    ? functionType(query, params, returnType)
    : { _type: Type.NAME, value: query }
}
