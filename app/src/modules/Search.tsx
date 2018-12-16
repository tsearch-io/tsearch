import * as React from 'react'
import styled from 'styled-components'

import Input from '../components/Input'
import Button from '../components/Button'
import SearchResult from '../components/SearchResult'
import { FunctionRecord } from '../models/tSearch'
import { search } from '../services/tSearch'

const Container = styled.div`
  font-family: sans-serif;
`

import {
  RemoteData,
  match,
  isLoading,
  notAsked,
  loading,
} from '../lib/remoteData'

interface State {
  data: RemoteData<FunctionRecord[]>
  query: string
}

const key = ({ location }: FunctionRecord) =>
  `${location.path}-${location.lines.from}-${location.lines.to}`

const renderData = match<FunctionRecord[], React.ReactNode>({
  notAsked: () => <span>search for function types</span>,
  loading: () => <span>loading ...</span>,
  failure: e => <span>{e}</span>,
  success: results =>
    results.map(result => <SearchResult key={key(result)} result={result} />),
})

export default class Search extends React.Component<{}, State> {
  state = {
    data: notAsked<FunctionRecord[]>(),
    query: '',
  }

  onChange = (e: { target: HTMLInputElement }) => {
    this.setState({ query: e.target.value })
  }

  search = () => {
    this.setState({ data: loading<FunctionRecord[]>() })

    search('query').fork(
      error => {
        this.setState({ data: error })
      },
      results => {
        this.setState({ data: results })
      },
    )
  }

  isSearchDisabled() {
    return isLoading(this.state.data) || this.state.query === ''
  }

  render() {
    const { data, query } = this.state

    return (
      <Container>
        <form>
          <Input
            value={query}
            onChange={this.onChange}
            disabled={isLoading(data)}
          />
          <Button onClick={this.search} disabled={this.isSearchDisabled()}>
            Search
          </Button>
        </form>
        <div>{renderData(data)}</div>
      </Container>
    )
  }
}
