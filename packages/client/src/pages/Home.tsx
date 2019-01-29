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

const QUERIES = {
  stringToString: {
    pathname: '/query',
    search: `?q=${encodeURIComponent('string => string')}`,
  },
  replace: {
    pathname: '/query',
    search: `?q=${encodeURIComponent('string | RegExp, string => string')}`,
  },
}

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
        <li>
          <RouterLink to={QUERIES.stringToString}>string => string</RouterLink>
        </li>
        <li>
          <RouterLink to={QUERIES.replace}>
            string | RegExp, string => string
          </RouterLink>
        </li>
      </ul>
    </div>
  </Container>
)

export default Home
