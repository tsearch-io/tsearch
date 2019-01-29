import * as React from 'react'
import { RouteComponentProps } from 'react-router-dom'

import Search from '../modules/Search'

const getQuery = (search: string) =>
  search ? decodeURIComponent(search.slice(1)).replace('q=', '') : ''

const SearchPage: React.SFC<RouteComponentProps> = ({ location }) => (
  <Search query={getQuery(location.search)} />
)

export default SearchPage
