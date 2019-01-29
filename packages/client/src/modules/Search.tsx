import * as React from 'react'
import styled from 'styled-components'
import { RemoteData, isLoading } from 'remote-data-ts'
import { Task } from '@ts-task/task'

import { FunctionRecord } from 'ts-earch-types'

import { search, reload } from '../services/tSearch'

import Form from './Form'
import ListRecords from './ListRecords'

const Container = styled.div({ padding: 10 })

type Data = RemoteData<FunctionRecord[], string>

interface Props {
  query?: string
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

  fetch = (fn: () => Task<Data, Error>) => {
    this.setState({ data: RemoteData.loading() })

    // TODO: don't do this here, server should be on charge
    reload()
      .chain(fn)
      .fork(
        error => {
          // tslint:disable-next-line no-console
          console.error(error)
          this.setState({ data: RemoteData.failure('Unknown Error') })
        },
        data => this.setState({ data }),
      )
  }

  search = (query: string) => this.fetch(() => search(query))

  render() {
    const { data } = this.state

    return (
      <Container>
        <Form
          initialQuery={this.props.query}
          isLoading={isLoading(data)}
          onSubmit={this.search}
        />
        <ListRecords records={data} />
      </Container>
    )
  }
}
