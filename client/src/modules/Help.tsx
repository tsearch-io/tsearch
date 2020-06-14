import * as React from 'react'

import {List} from '../components'

const Help: React.SFC = () => (
  <div>
    <p>Supported search:</p>
    <List>
      <li>
        By function name. Eg. <code>match</code>, <code>map</code>,{' '}
        <code>render</code>.
      </li>
      <li>
        <p>
          By type signature: a comma separated list of types of parameters and
          the returned type after a fat arrow (<code>=></code>).
        </p>
        <pre>string => RegExp</pre>
        <pre>string, number => string[]</pre>
        <pre>A[] => A</pre>
      </li>
    </List>
  </div>
)

export default Help
