import * as React from 'react'
import { RouteComponentProps } from 'react-router-dom'
import styled from 'styled-components'

import Form from '../modules/Form'

const Container = styled.div({
  padding: 10,
  maxWidth: '100vw',
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
})

const Home: React.SFC<RouteComponentProps> = ({ history }) => (
  <Container>
    <h1>ts-earch</h1>
    <Form
      onSubmit={query =>
        history.push({ pathname: '/query', search: `?q=${query}` })
      }
      isLoading={false}
    />
  </Container>
)

export default Home
