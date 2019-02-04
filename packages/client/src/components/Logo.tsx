import * as React from 'react'
import styled from 'styled-components'

import { RouterLink } from './Link'

const Link = styled(RouterLink)({
  color: '#333',
  fontFamily: 'Helvetica',
  '&:visited': { color: '#333' },
})

const Logo = () => (
  <h1>
    <Link to="/">tsearch</Link>
  </h1>
)

export default Logo
