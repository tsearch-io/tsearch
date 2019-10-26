export interface Param {
  name: string
  type: string
  isGeneric: boolean
}

export interface Location {
  path: string
  lines: {
    from: number
    to: number
  }
}

export interface FunctionRecord {
  name?: string
  docs?: string
  text?: string
  location: Location
  module: string
  signature: SignatureT
}

export interface Module {
  name: string
  fns: FunctionRecord[]
}

// --- Type

export interface Base {
  text: string
}

export interface Any {
  __tag: 'Any'
}

export interface Unknown {
  __tag: 'Unknown'
}

export interface Undefined {
  __tag: 'Undefined'
}

export interface LiteralString {
  __tag: 'LiteralString'
  value: string
}

export interface LiteralNumber {
  __tag: 'LiteralNumber'
  value: number
}

export interface LiteralBoolean {
  __tag: 'LiteralBoolean'
  value: boolean
}

export interface Primitive extends Base {
  __tag: 'Primitive'
  typeName: 'string' | 'number' | 'boolean'
}

export interface ArrayT extends Base {
  __tag: 'Array'
  elementsType: Type
}

export interface Union extends Base {
  __tag: 'Union'
  types: Type[]
}

export interface Intersection extends Base {
  __tag: 'Intersection'
  types: Type[]
}

export interface Tuple extends Base {
  __tag: 'Tuple'
  types: Type[]
}

export interface SignatureT {
  typeParameters: TypeParameter[]
  parameters: { name: string; type: Type }[]
  returnType: Type
}

export interface FunctionT extends Base {
  __tag: 'Function'
  signature: SignatureT
}

export interface HigherOrder extends Base {
  __tag: 'HigherOrder'
  arguments: Type[]
}

export interface Other extends Base {
  __tag: 'Other'
}

export type Type =
  | Any
  | Unknown
  | Undefined
  | LiteralString
  | LiteralNumber
  | LiteralBoolean
  | Primitive
  | ArrayT
  | Union
  | Intersection
  | Tuple
  | FunctionT
  | HigherOrder
  | Other

interface TypeMatcher<R> {
  Any: (v: Any) => R
  Unknown: (v: Unknown) => R
  Undefined: (v: Undefined) => R
  LiteralString: (v: LiteralString) => R
  LiteralNumber: (v: LiteralNumber) => R
  LiteralBoolean: (v: LiteralBoolean) => R
  Primitive: (v: Primitive) => R
  Array: (v: ArrayT) => R
  Union: (v: Union) => R
  Intersection: (v: Intersection) => R
  Tuple: (v: Tuple) => R
  Function: (v: FunctionT) => R
  HigherOrder: (v: HigherOrder) => R
  Other: (v: Other) => R
}

const matchType = <R>(m: TypeMatcher<R>) => (t: Type): R => {
  switch (t.__tag) {
    case 'Any':
      return m.Any(t)
    case 'Unknown':
      return m.Unknown(t)
    case 'Undefined':
      return m.Undefined(t)
    case 'LiteralString':
      return m.LiteralString(t)
    case 'LiteralNumber':
      return m.LiteralNumber(t)
    case 'LiteralBoolean':
      return m.LiteralBoolean(t)
    case 'Primitive':
      return m.Primitive(t)
    case 'Array':
      return m.Array(t)
    case 'Union':
      return m.Union(t)
    case 'Intersection':
      return m.Intersection(t)
    case 'Tuple':
      return m.Tuple(t)
    case 'Function':
      return m.Function(t)
    case 'HigherOrder':
      return m.HigherOrder(t)
    case 'Other':
      return m.Other(t)
    default:
      throw new Error(`Unrecognized Type: ${JSON.stringify(t)}`)
  }
}

export const stringifySignature = (s: SignatureT): string => {
  const typeParams = s.typeParameters.map(stringifyTypeParam).join(', ')
  const params = s.parameters
    .map(({ name, type }, i) => `${name || 't' + i}: ${stringifyType(type)}`)
    .join(', ')
  const returnType = stringifyType(s.returnType)

  return s.typeParameters.length === 0
    ? `(${params}) => ${returnType}`
    : `<${typeParams}>(${params}) => ${returnType}`
}

function stringifyType(t: Type): string {
  return matchType<string>({
    Any: () => 'any',
    Unknown: () => 'unknown',
    Undefined: () => 'undefined',
    LiteralString: ({ value }) => value,
    LiteralNumber: ({ value }) => value.toString(),
    LiteralBoolean: ({ value }) => value.toString(),
    Primitive: ({ typeName }) => typeName,
    Array: ({ elementsType }) => `${stringifyType(elementsType)}[]`,
    Union: ({ types }) => types.map(stringifyType).join(' | '),
    Intersection: ({ types }) => types.map(stringifyType).join(' & '),
    Tuple: ({ types }) => `[${types.map(stringifyType).join(', ')}]`,
    // TODO: handle several as well as none (which shouldn't the case)
    Function: ({ signature: s }) => stringifySignature(s),
    // TODO HigherOrder should have type name separated from parameters
    HigherOrder: ({ text }) => text,
    Other: ({ text }) => text,
  })(t)
}

export const Type = {
  match: matchType,
  isAny: (t: Type): t is Any => t.__tag === 'Any',
  isUnknown: (t: Type): t is Unknown => t.__tag === 'Unknown',
  isUndefined: (t: Type): t is Undefined => t.__tag === 'Undefined',
  isLiteralString: (t: Type): t is LiteralString => t.__tag === 'LiteralString',
  isLiteralNumber: (t: Type): t is LiteralNumber => t.__tag === 'LiteralNumber',
  isLiteralBoolean: (t: Type): t is LiteralBoolean =>
    t.__tag === 'LiteralBoolean',
  isPrimitive: (t: Type): t is Primitive => t.__tag === 'Primitive',
  isArray: (t: Type): t is ArrayT => t.__tag === 'Array',
  isUnion: (t: Type): t is Union => t.__tag === 'Union',
  isIntersection: (t: Type): t is Intersection => t.__tag === 'Intersection',
  isTuple: (t: Type): t is Tuple => t.__tag === 'Tuple',
  isFunction: (t: Type): t is FunctionT => t.__tag === 'Function',
  isHigherOrder: (t: Type): t is HigherOrder => t.__tag === 'HigherOrder',
  isOther: (t: Type): t is Other => t.__tag === 'Other',
  stringify: stringifyType,
}

// --- TypeParameters

// TODO: should have name separated from text
interface Constrained extends Base {
  __tag: 'Constrained'
  constraint: Type
}

// TODO: should have name separated from text
interface WithDefault extends Base {
  __tag: 'WithDefault'
  default: Type
}

interface Polymorphic extends Base {
  __tag: 'Polymorphic'
}

export type TypeParameter = Constrained | WithDefault | Polymorphic

interface TypeParameterMatcher<R> {
  Constrained: (v: Constrained) => R
  WithDefault: (v: WithDefault) => R
  Polymorphic: (v: Polymorphic) => R
}

const matchTypeParamer = <R>(m: TypeParameterMatcher<R>) => (
  tp: TypeParameter,
): R => {
  switch (tp.__tag) {
    case 'Constrained':
      return m.Constrained(tp)
    case 'WithDefault':
      return m.WithDefault(tp)
    case 'Polymorphic':
      return m.Polymorphic(tp)
    default:
      throw new Error(`Unrecognized Type: ${JSON.stringify(tp)}`)
  }
}

function stringifyTypeParam(t: TypeParameter): string {
  return matchTypeParamer<string>({
    Constrained: ({ text, constraint }) =>
      `${text} extends ${stringifyType(constraint)}`,
    WithDefault: ({ text, default: d }) => `${text} = ${stringifyType(d)}`,
    Polymorphic: ({ text }) => text,
  })(t)
}

export const TypeParameter = {
  match: matchTypeParamer,
  stringify: stringifyTypeParam,
}
