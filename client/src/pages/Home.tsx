import * as React from 'react'
import styled from 'styled-components'
import {RouteComponentProps} from 'react-router-dom'

import {List, Link, RouterLink} from '../components'
import Form from '../modules/Form'

const Queries = styled.div({
  maxWidth: 400,
  padding: 20,
  backgroundColor: 'rgba(0,0,0,.05)',
  marginTop: 10,
  marginBottom: 20,
})

const queries = [
  'A[] => A',
  'string => string',
  'number => number[]',
  'string, RegExp, string, string => string',
  'compose',
  'render',
  'createGlobalStyle',
]

const qToPath = (q: string) => ({
  pathname: '/query',
  search: `?q=${encodeURIComponent(q)}`,
})

const Home: React.SFC<RouteComponentProps> = ({history}) => (
  <>
    <Form
      onSubmit={q => history.push({pathname: '/query', search: `?q=${q}`})}
      isLoading={false}
    />
    <h3>Welcome to Tsearch</h3>
    <p>
      Tsearch is a TypeScript search engine. It allows to search functions from
      packages by name or aproximate type signature.
    </p>

    <Queries>
      <b>Example queries</b>
      <List>
        {queries.map(q => (
          <li key={q}>
            <code>
              <RouterLink to={qToPath(q)}>{q}</RouterLink>
            </code>
          </li>
        ))}
      </List>
    </Queries>

    <p>
      <b>Tsearch is under development</b>. It is only a proof of concept at the
      moment.
    </p>
    <p>
      Keep and eye on the{' '}
      <Link
        href="https://dev.to/gillchristian/a-crazy-idea-and-a-proof-of-concept-2oj7"
        target="_blank"
      >
        blog
      </Link>{' '}
      and the{' '}
      <Link href="https://github.com/gillchristian/ts-earch" target="_blank">
        repo
      </Link>{' '}
      for updates.
    </p>
  </>
)

export default Home
