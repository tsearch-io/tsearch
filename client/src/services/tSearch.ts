import {Task, UnknownError} from '@ts-task/task'
import {fetch, TaskFetch} from '@ts-task/fetch'
import * as queryString from 'query-string'
import {RemoteData, ResolvedData} from 'remote-data-ts'

import {FunctionRecord} from '../types'
import {SearchError, fetchError} from './SearchError'

interface ServerSuccess {
  data: FunctionRecord[]
}

interface ServerError {
  err: SearchError
}

type SearchResult = ResolvedData<FunctionRecord[], SearchError>

type SearchTask = Task<SearchResult, Error | UnknownError>

type FailureTask = Task<ServerError, UnknownError>

const response = (
  res: TaskFetch.Response,
): Task<SearchResult, Error | UnknownError> => {
  // All good !!! \o/
  if (res.ok) {
    return (res.json() as Task<ServerSuccess, UnknownError>)
      .map(({data}) => data)
      .map(RemoteData.success)
  }

  // Client error, sent by backend
  if (res.status === 400) {
    return (res.json() as FailureTask)
      .map(({err}) => err)
      .map(RemoteData.failure)
  }

  // Welp, something is really wrong =/
  return Task.resolve(
    RemoteData.failure(fetchError(`${res.status} ${res.statusText}`)),
  )
}

const base = process.env.REACT_APP_BASE_URL || 'http://localhost:9000'

export const search = (query: string): SearchTask =>
  fetch(`${base}/search?${queryString.stringify({query})}`).chain(response)
