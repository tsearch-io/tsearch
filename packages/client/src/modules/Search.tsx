import * as React from 'react'
import { RemoteData, isLoading, map } from 'remote-data-ts'
import * as prettier from 'prettier/standalone'
import * as tsParser from 'prettier/parser-typescript'

import { FunctionRecord, stringifySignature } from 'ts-earch-types'

import { search } from '../services/tSearch'

import Form from './Form'
import ListRecords from './ListRecords'
import { FormattedFunctionRecord } from '../types'

type Data = RemoteData<FormattedFunctionRecord[], string>

interface Props {
  query?: string
  onSearch: (q: string) => void
}

interface State {
  data: Data
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

const formatRecords = map<FunctionRecord[], string, FormattedFunctionRecord[]>(
  rs =>
    rs.map(r => ({
      ...r,
      formattedSignature: formatSignature(stringifySignature(r.signature)),
    })),
)

export default class Search extends React.Component<Props, State> {
  state = {
    data: RemoteData.notAsked(),
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
    this.setState({ data: RemoteData.loading() })

    search(query)
      .map(formatRecords)
      .fork(
        error => {
          // tslint:disable-next-line no-console
          console.error(error)
          this.setState({ data: RemoteData.failure('Unknown Error') })
        },
        data => this.setState({ data }),
      )
  }

  render() {
    const { data } = this.state

    return (
      <>
        <Form
          initialQuery={this.props.query}
          isLoading={isLoading(data)}
          onSubmit={this.onSubmit}
        />
        <ListRecords records={data} />
      </>
    )
  }
}
