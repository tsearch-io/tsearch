import * as React from 'react'
import {RouteComponentProps} from 'react-router-dom'

import Search from '../modules/Search'

const getQuery = (search: string) =>
  search ? decodeURIComponent(search.replace('?q=', '')) : ''

const SearchPage: React.SFC<RouteComponentProps> = ({location, history}) => (
  <Search
    query={getQuery(location.search)}
    onSearch={(q: string) =>
      history.push({pathname: '/query', search: `?q=${q}`})
    }
  />
)

export default SearchPage
