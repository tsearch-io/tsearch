import * as React from 'react'
import styled from 'styled-components'

import { FunctionRecord } from 'ts-earch-types'

import { search, reload, all } from '../services/tSearch'
import {
  RemoteData,
  isLoading,
  notAsked,
  loading,
  failure,
} from '../lib/remoteData'

import Form from './Form'
import ListRecords from './ListRecords'

const Container = styled.div`
  padding: 10px;
`

interface State {
  data: RemoteData<FunctionRecord[]>
}

export default class Search extends React.Component<{}, State> {
  state = {
    data: notAsked<FunctionRecord[]>(),
  }

  search = (query: string) => {
    this.setState({ data: loading<FunctionRecord[]>() })

    // TODO: don't do this here, server should be on charge
    reload()
      .map(() => query)
      .chain(search)
      .fork(
        error => {
          // tslint:disable-next-line no-console
          console.error(error)
          this.setState({ data: failure('Unknown Error') })
        },
        data => this.setState({ data }),
      )
  }

  loadAll = () => {
    this.setState({ data: loading<FunctionRecord[]>() })

    // TODO: don't do this here, server should be on charge
    reload()
      .chain(all)
      .fork(
        error => {
          // tslint:disable-next-line no-console
          console.error(error)
          this.setState({ data: failure('Unknown Error') })
        },
        data => this.setState({ data }),
      )
  }

  render() {
    const { data } = this.state

    return (
      <Container>
        <Form
          isLoading={isLoading(data)}
          onSubmit={this.search}
          onClickAll={this.loadAll}
        />
        <ListRecords records={data} />
      </Container>
    )
  }
}
