import * as React from 'react'

import Input from '../components/Input'
import Button from '../components/Button'

interface State {
  query: string
}

interface Props {
  isLoading: boolean
  onSubmit: (query: string) => void
  onClickAll: () => void
}

export default class Search extends React.Component<Props, State> {
  state = {
    query: '',
  }

  onChange = (e: { target: HTMLInputElement }) => {
    this.setState({ query: e.target.value })
  }

  onSubmit = (e: React.SyntheticEvent) => {
    e.preventDefault()

    this.props.onSubmit(this.state.query)
  }

  render() {
    const { query } = this.state
    const { isLoading } = this.props

    const noSubmit = isLoading || query === ''

    return (
      <form>
        <Input value={query} onChange={this.onChange} disabled={isLoading} />
        <Button onClick={this.onSubmit} disabled={noSubmit}>
          Search
        </Button>
        <Button onClick={this.props.onClickAll} disabled={isLoading}>
          List all
        </Button>
      </form>
    )
  }
}
