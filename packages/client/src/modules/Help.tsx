import * as React from 'react'

import { Link } from '../components'

const Help: React.SFC = () => (
  <div>
    <p>
      Currently tsearch is under development and it supports only a custom
      syntax for searching, either by types or by function name.
    </p>
    <ul>
      <li>
        <p>
          A comma separated list of types of parameters and the returned type
          after a fat arrow (<code>=></code>).
        </p>
        <pre>string => RegExp</pre>
        <pre>string, number => string[]</pre>
        <pre>A[] => A</pre>
      </li>
      <li>
        <p>
          If only one word is used, the search is done matching function names.
        </p>
        <pre>render</pre>
        <pre>match</pre>
        <pre>map</pre>
      </li>
    </ul>
    <p>
      Keep and eye on the{' '}
      <Link
        href="https://dev.to/gillchristian/a-crazy-idea-and-a-proof-of-concept-2oj7"
        target="_blank"
      >
        blog
      </Link>{' '}
      (<code>ts-earch devlog</code> series) and the{' '}
      <Link href="https://github.com/gillchristian/ts-earch" target="_blank">
        repo
      </Link>{' '}
      for updates.
    </p>
  </div>
)

export default Help
