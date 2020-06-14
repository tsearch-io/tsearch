import * as React from 'react'
import styled from 'styled-components'

import {RouterLink} from './Link'

const Link = styled(RouterLink)({
  display: 'flex',
  alignItems: 'flex-end',
  color: '#333',
  fontFamily: 'Helvetica',
  marginBottom: 20,
  '&:visited': {color: '#333'},
})

const H1 = styled.h1({margin: 0, marginLeft: 10, lineHeight: 1})

const Img = styled.img({width: 64, height: 64})

const Logo = () => (
  <Link to="/">
    <Img src="/logo.png" />
    <H1>tsearch</H1>
  </Link>
)

export default Logo
