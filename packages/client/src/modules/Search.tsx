import * as React from 'react'
import { RemoteData, isLoading, map } from 'remote-data-ts'
import * as prettier from 'prettier/standalone'
import * as tsParser from 'prettier/parser-typescript'

import { FunctionRecord, stringifySignature } from 'ts-earch-types'

import { search } from '../services/tSearch'

import Form from './Form'
import ListRecords from './ListRecords'
import { FormattedFunctionRecord } from '../types'
import { SearchError, unknownError } from '../services/SearchError'

type SearchResult = RemoteData<FormattedFunctionRecord[], SearchError>

interface Props {
  query?: string
  onSearch: (q: string) => void
}

interface State {
  searchResult: SearchResult
}

const formatSignature = (code: string) =>
  prettier
    .format(`type T = ${code}`, {
      parser: 'typescript',
      semi: false,
      singleQuote: true,
      trailingComma: 'all',
      bracketSpacing: true,
      arrowParens: 'always',
      plugins: [tsParser],
    })
    .replace(/^type T = /, '')

const formatRecords = map<
  FunctionRecord[],
  SearchError,
  FormattedFunctionRecord[]
>(rs =>
  rs.map(r => ({
    ...r,
    formattedSignature: formatSignature(stringifySignature(r.signature)),
  })),
)

export default class Search extends React.Component<Props, State> {
  state: State = {
    searchResult: RemoteData.notAsked(),
  }

  componentDidMount() {
    if (this.props.query) {
      this.search(this.props.query)
    }
  }

  onSubmit = (query: string) => {
    this.props.onSearch(query)
    this.search(query)
  }

  search = (query: string) => {
    this.setState({ searchResult: RemoteData.loading() })

    search(query)
      .map(formatRecords)
      .fork(
        error => {
          // tslint:disable-next-line no-console
          console.error(error)
          // All errors should've been handled by now
          this.setState({ searchResult: RemoteData.failure(unknownError()) })
        },
        data => {
          this.setState({ searchResult: data })
        },
      )
  }

  render() {
    return (
      <>
        <Form
          initialQuery={this.props.query}
          isLoading={isLoading(this.state.searchResult)}
          onSubmit={this.onSubmit}
        />
        <ListRecords records={this.state.searchResult} />
      </>
    )
  }
}
