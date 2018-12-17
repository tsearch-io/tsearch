import * as React from 'react'
import { render } from 'react-dom'

import Search from './modules/Search'

class App extends React.Component {
  render() {
    return <Search />
  }
}

render(<App />, document.getElementById('root'))
