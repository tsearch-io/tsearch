export type SearchError =
  // Client Errors - 400 code
  | {tag: 'InvalidQuery'; contents: string}
  | {tag: 'MissingQuery'}
  // Other Errors (e.g. network down, server error !== 400)
  | {tag: 'FetchError'; message: string}
  | {tag: 'UnknownError'}

export const invalidQuery = (contents: string): SearchError => ({
  tag: 'InvalidQuery',
  contents,
})

export const missingQuery = (): SearchError => ({tag: 'MissingQuery'})

export const fetchError = (message: string): SearchError => ({
  tag: 'FetchError',
  message,
})

export const unknownError = (): SearchError => ({tag: 'UnknownError'})

interface Handler<R> {
  InvalidQuery: (contents: string) => R
  MissingQuery: () => R
  FetchError: (message: string) => R
  UnknownError: () => R
}

export const fold = <R>(handler: Handler<R>) => (fa: SearchError): R => {
  if (fa.tag === 'InvalidQuery') {
    return handler.InvalidQuery(fa.contents)
  }
  if (fa.tag === 'MissingQuery') {
    return handler.MissingQuery()
  }
  if (fa.tag === 'FetchError') {
    return handler.FetchError(fa.message)
  }
  return handler.UnknownError()
}
