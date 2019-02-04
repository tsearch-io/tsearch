import * as React from 'react'
import { RouteComponentProps } from 'react-router-dom'
import styled from 'styled-components'

import { Col, Link, Logo } from '../components'
import Search from '../modules/Search'

const Container = styled.div({
  display: 'flex',
  margin: '0 auto',
  maxWidth: 1366,
  '@media screen and (max-width: 1024px)': {
    flexDirection: 'column',
  },
})

const SearchContainer = styled.div({
  padding: 10,
  paddingTop: 20,
})

const Sidebar = styled.div({
  display: 'flex',
  flexDirection: 'column',
  paddingLeft: 20,
})

const getQuery = (search: string) =>
  search ? decodeURIComponent(search.replace('?q=', '')) : ''

const SearchPage: React.SFC<RouteComponentProps> = ({ location, history }) => (
  <Container>
    <Col w={15}>
      <Sidebar>
        <Logo />
        <Link href="https://github.com/gillchristian/ts-earch" target="_blank">
          tsearch on GitHub
        </Link>
        <Link href="https://www.typescriptlang.org/" target="_blank">
          typescriptlang.org
        </Link>
        <Link
          href="https://dev.to/gillchristian/a-crazy-idea-and-a-proof-of-concept-2oj7"
          target="_blank"
        >
          Blog
        </Link>
      </Sidebar>
    </Col>
    <Col w={90}>
      <SearchContainer>
        <Search
          query={getQuery(location.search)}
          onSearch={(q: string) =>
            history.push({ pathname: '/query', search: `?q=${q}` })
          }
        />
      </SearchContainer>
    </Col>
  </Container>
)

export default SearchPage
