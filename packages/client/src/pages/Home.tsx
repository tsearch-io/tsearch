import * as React from 'react'
import { RouteComponentProps } from 'react-router-dom'
import styled from 'styled-components'

import { Logo, RouterLink } from '../components'
import Form from '../modules/Form'

const Container = styled.div({
  padding: 10,
  maxWidth: '100vw',
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
})

const queries = [
  'string => string',
  'number => number[]',
  'string, RegExp, string, string => string',
]

const qToPath = (q: string) => ({
  pathname: '/query',
  search: `?q=${encodeURIComponent(q)}`,
})

const Home: React.SFC<RouteComponentProps> = ({ history }) => (
  <Container>
    <Logo />
    <Form
      onSubmit={query =>
        history.push({ pathname: '/query', search: `?q=${query}` })
      }
      isLoading={false}
    />
    <div>
      <h4>Search TypeScript functions and method by type signature or name</h4>
      <p>
        <b>Example queries</b>
      </p>
      <ul>
        {queries.map(q => (
          <li key={q}>
            <RouterLink to={qToPath(q)}>{q}</RouterLink>
          </li>
        ))}
      </ul>
    </div>
  </Container>
)

export default Home
