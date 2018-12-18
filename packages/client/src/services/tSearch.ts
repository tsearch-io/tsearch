import { Task, UnknownError } from '@ts-task/task'
import { fetch } from '@ts-task/fetch'
import * as queryString from 'query-string'

import { FunctionRecord } from 'ts-earch-types'

type SearchTask = Task<FunctionRecord[], TypeError | UnknownError>

export const search = (query: string): SearchTask =>
  fetch(
    `http://localhost:8080/search?${queryString.stringify({ query })}`,
  ).chain(res => res.json())

export const reload = (): Task<void, TypeError | UnknownError> =>
  fetch('http://localhost:8080/search/reload').chain(res =>
    res.ok
      ? Task.resolve(undefined)
      : Task.reject(new Error('Failed to reload types')),
  )
