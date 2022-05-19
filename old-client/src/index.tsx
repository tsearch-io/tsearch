import * as React from 'react'
import {render} from 'react-dom'
import {Route, Router, Switch} from 'react-router-dom'
import createHistory from 'history/createBrowserHistory'
import {createGlobalStyle} from 'styled-components'

import Home from './pages/Home'
import Search from './pages/Search'
import {Layout} from './components'

const history = createHistory({
  basename: process.env.REACT_APP_BASENAME || '',
})

const GlobalStyles = createGlobalStyle({
  body: {
    margin: 0,
    padding: 0,
    fontFamily: 'sans-serif',
    maxWidth: '100vw',
  },
})

const Root: React.SFC = () => (
  <>
    <Router history={history}>
      <Layout>
        <Switch>
          <Route path="/" exact={true} component={Home} />
          <Route path="/query" component={Search} />
        </Switch>
      </Layout>
      <GlobalStyles />
    </Router>
  </>
)

render(<Root />, document.getElementById('root'))
