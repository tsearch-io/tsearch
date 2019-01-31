import { readFileSync } from 'fs'
import * as fs from '@ts-task/fs'

import { FunctionRecord, Module } from 'ts-earch-types'

export const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B) => (a: A) =>
  f(g(a))

const parse = (fileContent: string): FunctionRecord[] =>
  (JSON.parse(fileContent) as Module[])
    .filter(({ fns }) => fns.length > 0)
    .reduce((acc: FunctionRecord[], { fns }: Module) => {
      acc.push(...fns)

      return acc
    }, [])

export const getTypes = (p: string) => fs.readFile(p, 'utf-8').map(parse)
export const getTypesSync = compose(
  parse,
  (p: string) => readFileSync(p, 'utf-8'),
)
