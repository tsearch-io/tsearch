import * as React from 'react'
import { render } from 'react-dom'
import { createGlobalStyle } from 'styled-components'

import Search from './modules/Search'

const GlobalStyles = createGlobalStyle`
  body {
    margin: 0;
    padding: 0;

    font-family: sans-serif;

    max-width: 100vw;
  }
`

class App extends React.Component {
  render() {
    return (
      <>
        <Search />
        <GlobalStyles />
      </>
    )
  }
}

render(<App />, document.getElementById('root'))
