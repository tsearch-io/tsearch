import React, {useState, useEffect, FC} from 'react'
import {RemoteData, is, map} from 'remote-data-ts'

import {FunctionRecord, stringifySignature} from '../types'

import {search} from '../services/tSearch'

import Form from './Form'
import ListRecords from './ListRecords'
import {FormattedFunctionRecord} from '../types'
import {SearchError, unknownError} from '../services/SearchError'

type SearchResult = RemoteData<FormattedFunctionRecord[], SearchError>

interface Props {
  query?: string
  onSearch: (q: string) => void
}

const formatRecords = map<
  FunctionRecord[],
  SearchError,
  FormattedFunctionRecord[]
>(rs =>
  rs.map(r => ({
    ...r,
    formattedSignature: `type ${r.name || 't'} = ${stringifySignature(
      r.signature,
    )}`,
  })),
)

const Search: FC<Props> = ({query, onSearch}) => {
  const [searchResult, setSearchResult] = useState<SearchResult>(
    RemoteData.notAsked(),
  )

  const doSearch = (query: string) => {
    setSearchResult(RemoteData.loading())

    search(query)
      .map(formatRecords)
      .fork(error => {
        // tslint:disable-next-line no-console
        console.error(error)
        // All errors should've been handled by now
        setSearchResult(RemoteData.failure(unknownError()))
      }, setSearchResult)
  }

  const onSubmit = (query: string) => {
    onSearch(query)
    doSearch(query)
  }

  useEffect(() => {
    if (query) {
      doSearch(query)
    }
  }, []) // eslint-disable-line react-hooks/exhaustive-deps

  return (
    <>
      <Form
        initialQuery={query}
        isLoading={is.loading(searchResult)}
        onSubmit={onSubmit}
      />
      <h2>Search results</h2>
      <ListRecords query={query || ''} records={searchResult} />
    </>
  )
}

export default Search
