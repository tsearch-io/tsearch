import { FunctionRecord } from 'ts-earch-types'

import { parseQuery, isFunc, isName, weighFunctionRecord } from './index'

const partialParam: FunctionRecord = {
  name: 'multi_params_to_string',
  text: '',
  parameters: [
    { name: 'n', type: 'number' },
    { name: 'b', type: 'boolean' },
    { name: 's', type: 'string' },
  ],
  returnType: 'string',
  module: 'some_module',
  location: {
    path: '',
    lines: { from: 40, to: 40 },
  },
}
const noParam: FunctionRecord = {
  name: 'number_to_string',
  parameters: [{ name: 'n', type: 'number' }],
  returnType: 'string',
  module: 'some_module',
  location: {
    path: '',
    lines: { from: 5, to: 31 },
  },
}

const queries = {
  func: 'boolean => string',
  name: 'some_name',
}

describe('src/search', () => {
  describe('parseQuery', () => {
    it('parses a function query', () => {
      expect.assertions(2)
      const q = parseQuery(queries.func)

      if (isFunc(q)) {
        expect(q.parameters).toEqual(['boolean'])
        expect(q.returnType).toBe('string')
      }
    })

    it('parses a name query', () => {
      expect.assertions(1)
      const q = parseQuery(queries.name)

      if (isName(q)) {
        expect(q.value).toBe('some_name')
      }
    })
  })

  describe('weighFunctionRecord', () => {
    it('weighs partial param match and returnType match', () => {
      const q = parseQuery(queries.func)

      if (isFunc(q)) {
        const [, weight] = weighFunctionRecord(q)(partialParam)

        // weight === 2.333
        expect(weight > 2.3 && weight < 2.4).toBe(true)
      }
    })

    it('weighs no param match and returnType match', () => {
      const q = parseQuery(queries.func)

      if (isFunc(q)) {
        const [, weight] = weighFunctionRecord(q)(noParam)

        expect(weight).toBe(2)
      }
    })
  })
})
