import * as ts from 'ts-morph'

import { Type, SignatureT, TypeParameter } from 'ts-earch-types'

export function type(t: ts.Type<ts.ts.Type>): Type {
  const text = t.getText()

  if (t.isAny()) {
    return { __tag: 'Any' }
  }

  if (t.isUnknown()) {
    return { __tag: 'Unknown' }
  }

  if (t.isUndefined()) {
    return { __tag: 'Undefined' }
  }

  if (t.isArray()) {
    const elemType = t.getArrayElementType()

    return elemType
      ? { __tag: 'Array', text, elementsType: type(elemType) }
      : { __tag: 'Array', text, elementsType: { __tag: 'Any' } }
  }

  if (t.isBooleanLiteral() || t.isStringLiteral() || t.isNumberLiteral()) {
    return { __tag: 'LiteralPrimitive', text }
  }

  if (t.isString()) {
    return { __tag: 'Primitive', text, typeName: 'string' }
  }

  if (t.isNumber()) {
    return { __tag: 'Primitive', text, typeName: 'number' }
  }

  if (t.isBoolean()) {
    return { __tag: 'Primitive', text, typeName: 'boolean' }
  }

  if (t.isUnion()) {
    return { __tag: 'Union', text, types: t.getUnionTypes().map(type) }
  }

  if (t.isIntersection()) {
    return { __tag: 'Union', text, types: t.getIntersectionTypes().map(type) }
  }

  if (t.isTuple()) {
    return { __tag: 'Tuple', text, types: t.getTupleElements().map(type) }
  }

  // TODO: Is this the right way to know if it's function ?
  // const css = t.getCallSignatures()
  // if (css.length > 0) {
  //   return {
  //     __tag: 'function',
  //     text,
  //     signatures: css.map(s =>
  //       callSignature({
  //         parameters: s.getParameters(),
  //         typeParameters: s.getTypeParameters(),
  //         returnType: s.getReturnType(),
  //       }),
  //     ),
  //   }
  // }

  // TODO: not recognized as HigherOrder `Record<string, string | string[]>`
  const typeArgs = t.getTypeArguments()
  if (typeArgs.length > 0) {
    return { __tag: 'HigherOrder', text, arguments: typeArgs.map(type) }
  }

  // TODO: `Foo | null` -> is not considered union ?
  return { __tag: 'Other', text }
}

interface SignatureArgs {
  typeParameters: ts.TypeParameterDeclaration[]
  parameters: ts.ParameterDeclaration[]
  returnType: ts.Type
}

export function callSignature({
  parameters,
  typeParameters,
  returnType,
}: SignatureArgs): SignatureT {
  return {
    parameters: parameters.map(p => ({
      name: p.getName(),
      type: type(p.getType()),
    })),
    typeParameters: typeParameters.map(p => typeParameter(p.getType())),
    returnType: type(returnType),
  }
}

export function typeParameter(tp: ts.TypeParameter): TypeParameter {
  const constraint = tp.getConstraint()

  // TODO: text is not the name !!!
  const base = { text: tp.getText() }

  if (constraint) {
    return { ...base, __tag: 'Constrained', constraint: type(constraint) }
  }

  const default_ = tp.getDefault()

  if (default_) {
    return { ...base, __tag: 'WithDefault', default: type(default_) }
  }

  return { ...base, __tag: 'Polymorphic' }
}
