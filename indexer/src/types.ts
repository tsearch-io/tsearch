import * as ts from 'ts-morph'

export interface Location {
  path: string
  lines: {from: number; to: number}
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

interface Param {
  name: string
  type: Type
}

export interface SignatureT {
  parameters: Param[]
  returnType: Type
}

// --- Type

export interface Any {
  __tag: 'Any'
}

export interface Unknown {
  __tag: 'Unknown'
}

export interface Never {
  __tag: 'Never'
}

export interface Void {
  __tag: 'Void'
}

export interface Undefined {
  __tag: 'Undefined'
}

export interface Null {
  __tag: 'Null'
}

export interface LiteralString {
  __tag: 'LiteralString'
  values: string
}

export interface LiteralNumber {
  __tag: 'LiteralNumber'
  values: number
}

export interface LiteralBool {
  __tag: 'LiteralBool'
  values: boolean
}

export interface Bool {
  __tag: 'BoolT'
}

export interface StringT {
  __tag: 'StringT'
}

export interface NumberT {
  __tag: 'NumberT'
}

export interface Other {
  __tag: 'Other'
  values: string
}

export type Type =
  | Any
  | Unknown
  | Never
  | Void
  | Undefined
  | Null
  | LiteralString
  | LiteralNumber
  | LiteralBool
  | Bool
  | StringT
  | NumberT
  | Other

interface TypeMatcher<R> {
  Any: (v: Any) => R
  Unknown: (v: Unknown) => R
  Never: (v: Never) => R
  Void: (v: Void) => R
  Undefined: (v: Undefined) => R
  Null: (v: Null) => R
  LiteralString: (v: LiteralString) => R
  LiteralNumber: (v: LiteralNumber) => R
  LiteralBool: (v: LiteralBool) => R
  Bool: (v: Bool) => R
  StringT: (v: StringT) => R
  NumberT: (v: NumberT) => R
  Other: (v: Other) => R
}

const matchType = <R>(m: TypeMatcher<R>) => (t: Type): R => {
  switch (t.__tag) {
    case 'Any':
      return m.Any(t)
    case 'Unknown':
      return m.Unknown(t)
    case 'Void':
      return m.Void(t)
    case 'Never':
      return m.Never(t)
    case 'Undefined':
      return m.Undefined(t)
    case 'Null':
      return m.Null(t)
    case 'LiteralString':
      return m.LiteralString(t)
    case 'LiteralNumber':
      return m.LiteralNumber(t)
    case 'LiteralBool':
      return m.LiteralBool(t)
    case 'BoolT':
      return m.Bool(t)
    case 'StringT':
      return m.StringT(t)
    case 'NumberT':
      return m.NumberT(t)
    case 'Other':
      return m.Other(t)
    default:
      throw new Error(`Unrecognized Type: ${JSON.stringify(t)}`)
  }
}

export const stringifySignature = (s: SignatureT): string => {
  const params = s.parameters
    .map(({name, type}, i) => `${name || 't' + i}: ${stringifyType(type)}`)
    .join(', ')
  const returnType = stringifyType(s.returnType)

  return `(${params}) => ${returnType}`
}

function stringifyType(t: Type): string {
  return matchType<string>({
    Any: () => 'any',
    Unknown: () => 'unknown',
    Void: () => 'void',
    Never: () => 'never',
    Undefined: () => 'undefined',
    Null: () => 'null',
    LiteralString: ({values}) => values,
    LiteralNumber: ({values}) => values.toString(),
    LiteralBool: ({values}) => values.toString(),
    Bool: () => 'boolean',
    StringT: () => 'string',
    NumberT: () => 'number',
    Other: ({values}) => values,
  })(t)
}

export const Type = {
  match: matchType,
  isAny: (t: Type): t is Any => t.__tag === 'Any',
  isUnknown: (t: Type): t is Unknown => t.__tag === 'Unknown',
  isNever: (t: Type): t is Never => t.__tag === 'Never',
  isVoid: (t: Type): t is Void => t.__tag === 'Void',
  isUndefined: (t: Type): t is Undefined => t.__tag === 'Undefined',
  isNull: (t: Type): t is Null => t.__tag === 'Null',
  isLiteralString: (t: Type): t is LiteralString => t.__tag === 'LiteralString',
  isLiteralNumber: (t: Type): t is LiteralNumber => t.__tag === 'LiteralNumber',
  isLiteralBool: (t: Type): t is LiteralBool => t.__tag === 'LiteralBool',
  isBool: (t: Type): t is Bool => t.__tag === 'BoolT',
  isString: (t: Type): t is StringT => t.__tag === 'StringT',
  isNumber: (t: Type): t is NumberT => t.__tag === 'NumberT',
  isOther: (t: Type): t is Other => t.__tag === 'Other',
  stringify: stringifyType,
}

export function type(t: ts.Type<ts.ts.Type>): Type {
  const text = t.getText() || ''

  const flags = t.getFlags()

  if (t.isNull()) {
    return {__tag: 'Null'}
  }

  if (ts.ts.TypeFlags.Void === flags) {
    return {__tag: 'Void'}
  }

  if (ts.ts.TypeFlags.Never === flags) {
    return {__tag: 'Never'}
  }

  if (t.isAny()) {
    return {__tag: 'Any'}
  }

  if (t.isUnknown()) {
    return {__tag: 'Unknown'}
  }

  if (t.isUndefined()) {
    return {__tag: 'Undefined'}
  }

  if (t.isBooleanLiteral()) {
    return {__tag: 'LiteralBool', values: text === 'true'}
  }

  if (t.isNumberLiteral()) {
    return {__tag: 'LiteralNumber', values: parseFloat(text)}
  }

  if (t.isStringLiteral()) {
    return {__tag: 'LiteralString', values: text}
  }

  if (t.isString()) {
    return {__tag: 'StringT'}
  }

  if (t.isNumber()) {
    return {__tag: 'NumberT'}
  }

  if (t.isBoolean()) {
    return {__tag: 'BoolT'}
  }

  return {__tag: 'Other', values: text}
}

interface SignatureArgs {
  typeParameters: ts.TypeParameterDeclaration[]
  parameters: ts.ParameterDeclaration[]
  returnType: ts.Type
}

export function callSignature({
  parameters,
  returnType,
}: SignatureArgs): SignatureT {
  return {
    parameters: parameters.map(p => ({
      name: p.getName(),
      type: type(p.getType()),
    })),
    returnType: type(returnType),
  }
}
