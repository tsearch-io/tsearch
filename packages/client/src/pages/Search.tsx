import * as React from 'react'
import { RouteComponentProps } from 'react-router-dom'
import styled from 'styled-components'

import { Col, Link, Logo } from '../components'
import Search from '../modules/Search'

const Container = styled.div({
  display: 'flex',
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
    <Col w={10}>
      <Sidebar>
        <Logo />
        <p>
          <b>Links</b>
        </p>
        <Link href="https://github.com/gillchristian/ts-earch" target="_blank">
          ts-earch on GitHub
        </Link>
        <Link href="https://www.typescriptlang.org/" target="_blank">
          typescriptlang.org
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
