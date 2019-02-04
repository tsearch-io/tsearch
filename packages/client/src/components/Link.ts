import styled from 'styled-components'
import { Link as RRouterLink } from 'react-router-dom'

const none = {
  textDecoration: 'none',
  color: '#294E80',
}

const Link = styled.a({
  ...none,
  marginBottom: 10,
  color: '#294E80',
  '&:hover': none,
  '&:link': none,
  '&:active': none,
  '&:visited': none,
})

export default Link

export const RouterLink = Link.withComponent(RRouterLink)
