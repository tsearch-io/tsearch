import { Task } from '@ts-task/task'

import { FunctionRecord } from '../models/tSearch'
import { ResolvedData, success, failure } from '../lib/remoteData'

const results = [
  {
    name: 'reverse',
    parameters: [
      {
        name: 'a',
        type: 'string',
      },
    ],
    returnType: 'string',
    location: {
      path: '/home/gillchristian/dev/ts-earch/test/index.ts',
      lines: {
        from: 1,
        to: 6,
      },
    },
  },
  {
    name: 'split',
    parameters: [
      {
        name: 'a',
        type: 'string',
      },
      {
        name: 'param',
        type: 'string',
      },
    ],
    returnType: 'string[]',
    location: {
      path: '/home/gillchristian/dev/ts-earch/test/index.ts',
      lines: {
        from: 8,
        to: 10,
      },
    },
  },
  {
    name: 'add',
    parameters: [
      {
        name: 'a',
        type: 'number',
      },
      {
        name: 'b',
        type: 'number',
      },
    ],
    returnType: 'number',
    location: {
      path: '/home/gillchristian/dev/ts-earch/test/index.ts',
      lines: {
        from: 12,
        to: 12,
      },
    },
  },
  {
    name: 'email',
    parameters: [
      {
        name: 'user',
        type: 'User',
      },
    ],
    returnType: 'string',
    location: {
      path: '/home/gillchristian/dev/ts-earch/test/index.ts',
      lines: {
        from: 19,
        to: 20,
      },
    },
  },
  {
    name: 'greet',
    parameters: [
      {
        name: 'param1',
        type: '{ name: string; lastName: string; }',
      },
    ],
    returnType: 'string',
    location: {
      path: '/home/gillchristian/dev/ts-earch/test/index.ts',
      lines: {
        from: 22,
        to: 23,
      },
    },
  },
  {
    name: 'compose',
    parameters: [
      {
        name: 'f',
        type: '(b: B) => C',
      },
      {
        name: 'g',
        type: '(a: A) => B',
      },
    ],
    returnType: '(a: A) => C',
    location: {
      path: '/home/gillchristian/dev/ts-earch/test/index.ts',
      lines: {
        from: 25,
        to: 26,
      },
    },
  },
]

type Result = ResolvedData<FunctionRecord[]>

export const search = (query: string): Task<Result, Result> =>
  new Task((resolve, reject) => {
    setTimeout(
      _ =>
        Math.random() < 0.7
          ? resolve(success<FunctionRecord[]>(results))
          : reject(failure<FunctionRecord[]>('Failed to fetch')),
      1000,
    )
  })
