import * as React from 'react'
import {RemoteData, cata} from 'remote-data-ts'
import styled from 'styled-components'

import SearchResult from '../components/SearchResult'
import {FormattedFunctionRecord} from '../types'
import {SearchError, fold} from '../services/SearchError'

const Pre = styled.pre({margin: 0})

const key = ({location}: FormattedFunctionRecord) =>
  `${location.path}-${location.lines.from}-${location.lines.to}`

type RecordsResult = RemoteData<FormattedFunctionRecord[], SearchError>

interface Props {
  query: string
  records: RecordsResult
}

const Results: React.SFC<Props> = ({records, query}) => (
  <div>
    {cata<FormattedFunctionRecord[], SearchError, React.ReactNode>({
      notAsked: () => (
        <span>Search for functions by name, types or signature</span>
      ),
      loading: () => <span>Loading...</span>,
      failure: e =>
        fold({
          MissingQuery: () => <span>No search query.</span>,
          InvalidQuery: reason => (
            <>
              <p>Invalid Query: </p>
              <Pre>
                "{query}"{'\n\n'}Error on{' '}
                {/* First line of the error is: // '(line 1, column 5)' */
                reason.replace(/line \d, /, '').replace(/\n/g, ' ')}
              </Pre>
            </>
          ),
          FetchError: msg => <span>{msg}</span>,
          UnknownError: () => <span>Something went really wrong :(</span>,
        })(e),
      success: results =>
        results.length ? (
          results.map(r => <SearchResult key={key(r)} result={r} />)
        ) : (
          <span>
            No results for <b>{query}</b>.
          </span>
        ),
    })(records)}
  </div>
)

export default Results
