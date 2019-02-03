import { Task, UnknownError } from '@ts-task/task'
import { fetch, TaskFetch } from '@ts-task/fetch'
import * as queryString from 'query-string'
import { RemoteData, ResolvedData } from 'remote-data-ts'

import { FunctionRecord } from 'ts-earch-types'

interface ServerResponse {
  data: FunctionRecord[]
}

type SearchTask = Task<ResolvedData<FunctionRecord[], string>, UnknownError>

const response = <Data>(res: TaskFetch.Response) =>
  res.ok
    ? (res.json() as Task<Data, Error>) // TODO: use io-ts
    : Task.reject(new Error(`${res.status} ${res.statusText}`))

const base = process.env.REACT_APP_BASE_URL || 'http://localhost:8080'

export const search = (query: string): SearchTask =>
  fetch(`${base}/search?${queryString.stringify({ query })}`)
    .chain(response)
    .map((fns: ServerResponse) => fns.data)
    .map(RemoteData.success)
    .catch(err => Task.resolve(RemoteData.failure(err.message)))
