interface Success<Data> {
  readonly _type: 'success'
  readonly data: Data
}

interface Failure {
  readonly _type: 'failure'
  readonly msg: string
}

type NotAsked = 'notAsked'
type Loading = 'loading'

// promises should resolve with this type
export type ResolvedData<D> = Failure | Success<D>
export type RemoteData<D> = NotAsked | Loading | ResolvedData<D>

export const notAsked = <D>(): RemoteData<D> => 'notAsked'
export const loading = <D>(): RemoteData<D> => 'loading'
export const failure = <D>(msg: string): Failure => ({
  _type: 'failure',
  msg,
})
export const success = <D>(data: D): Success<D> => ({
  _type: 'success',
  data,
})

interface Matcher<D, R> {
  notAsked: () => R
  loading: () => R
  failure: (msg: string) => R
  success: (data: D) => R
}

export const match = <D, R = void>(m: Matcher<D, R>) => (
  rd: RemoteData<D>,
): R => {
  if (rd === 'notAsked') {
    return m.notAsked()
  }
  if (rd === 'loading') {
    return m.loading()
  }
  if (rd._type === 'failure') {
    return m.failure(rd.msg)
  }
  return m.success(rd.data)
}

type Mapper<D, R> = (d: D) => R
export const map = <D, R>(fn: Mapper<D, R>) =>
  match<D, RemoteData<R>>({
    notAsked,
    loading,
    failure,
    success: data => success(fn(data)),
  })

type Reducer<D, R> = (acc: R, d: D) => R
export const reduce = <D, R>(fn: Reducer<D, R>) => (def: R) =>
  match<D, R>({
    notAsked: () => def,
    loading: () => def,
    failure: () => def,
    success: data => fn(def, data),
  })

type Chainer<D, R> = (d: D) => RemoteData<R>
export const chain = <D, R>(fn: Chainer<D, R>) =>
  match<D, RemoteData<R>>({
    notAsked,
    loading,
    failure,
    success: data => fn(data),
  })

export const isLoaded = match({
  notAsked: () => false,
  loading: () => false,
  failure: () => true,
  success: () => true,
})

export const isLoading = match({
  notAsked: () => false,
  loading: () => true,
  failure: () => false,
  success: () => false,
})
