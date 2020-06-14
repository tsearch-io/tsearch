import * as React from 'react'
import styled from 'styled-components'

import Col from './Col'
import Link from './Link'
import Logo from './Logo'

const Site = styled.div({
  display: 'flex',
  flexDirection: 'column',
  minHeight: '100vh',
  margin: '0 auto',
})

const SiteContent = styled.div({
  flex: 1,
  display: 'flex',
  '@media screen and (max-width: 1024px)': {
    flexDirection: 'column',
  },
})

const Main = styled.div({
  padding: 10,
  paddingTop: 20,
})

const Sidebar = styled.div({
  display: 'flex',
  flexDirection: 'column',
  paddingLeft: 20,
  paddingTop: 20,
})

const Footer = styled.footer({
  width: '100vw',
  display: 'flex',
  justifyContent: 'center',
})

const Layout: React.SFC = ({children}) => (
  <Site>
    <SiteContent>
      <Col w={20}>
        <Sidebar>
          <Logo />
          <Link
            href="https://github.com/gillchristian/ts-earch"
            target="_blank"
          >
            tsearch on GitHub
          </Link>
          <Link href="https://www.typescriptlang.org/" target="_blank">
            typescriptlang.org
          </Link>
          <Link
            href="https://gillchristian.xyz/articles/tsearch"
            target="_blank"
          >
            Blog
          </Link>
        </Sidebar>
      </Col>
      <Col w={80}>
        <Main>{children}</Main>
      </Col>
    </SiteContent>
    <Footer>
      <p>
        <Link
          href="https://gillchristian.xyz"
          target="_blank"
          rel="noopener noreferrer"
        >
          Christian Gill
        </Link>{' '}
        &copy; 2019-2020
      </p>
    </Footer>
  </Site>
)

export default Layout
