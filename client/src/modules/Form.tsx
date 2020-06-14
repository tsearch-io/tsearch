import React, {FC, useState} from 'react'
import styled from 'styled-components'

import {Input, Button, Collapse} from '../components'

import Help from './Help'

const MonoInput = styled(Input)({fontFamily: 'monospace'})

interface Props {
  initialQuery?: string
  isLoading: boolean
  onSubmit: (query: string) => void
}

const Search: FC<Props> = ({initialQuery, isLoading, onSubmit}) => {
  const [query, setQuery] = useState(initialQuery || '')

  const onChange = (e: {target: HTMLInputElement}) => setQuery(e.target.value)

  const handleSubmit = (e: React.SyntheticEvent) => {
    e.preventDefault()

    if (query.trim() !== '') {
      onSubmit(query)
    }
  }

  const noSubmit = isLoading || query.trim() === ''

  return (
    <Collapse
      trigger={({toggle}) => (
        <form>
          <MonoInput
            value={query}
            onChange={onChange}
            disabled={isLoading}
            placeholder="Search for..."
          />
          <Button onClick={handleSubmit} disabled={noSubmit}>
            Search
          </Button>
          <Button onClick={toggle} type="button">
            Help
          </Button>
        </form>
      )}
    >
      <Help />
    </Collapse>
  )
}

export default Search
