import * as React from 'react'
import { RemoteData, isLoading } from 'remote-data-ts'
import { Task } from '@ts-task/task'

import { FunctionRecord } from 'ts-earch-types'

import { search } from '../services/tSearch'

import Form from './Form'
import ListRecords from './ListRecords'

type Data = RemoteData<FunctionRecord[], string>

interface Props {
  query?: string
  onSearch: (q: string) => void
}

interface State {
  data: Data
}

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

    search(query).fork(
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
