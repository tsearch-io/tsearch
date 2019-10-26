import * as React from 'react'
import { RemoteData, cata } from 'remote-data-ts'
import styled from 'styled-components'

import SearchResult from '../components/SearchResult'
import { FormattedFunctionRecord } from '../types'
import { SearchError, fold } from '../services/SearchError'

const Flex = styled.div({
  display: 'flex',
  flexWrap: 'wrap',
})

const Pre = styled.pre({
  margin: 0,
  marginLeft: 10,
})

const key = ({ location }: FormattedFunctionRecord) =>
  `${location.path}-${location.lines.from}-${location.lines.to}`

const renderData = cata<
  FormattedFunctionRecord[],
  SearchError,
  React.ReactNode
>({
  notAsked: () => <span>Search for functions by name, types or signature</span>,
  loading: () => <span>loading ...</span>,
  failure: e =>
    fold({
      MissingQuery: () => <span>Missing query</span>,
      InvalidQuery: reason => (
        <Flex>
          <p>Invalid Query: </p>
          <p>
            <Pre>
              Error on{' '}
              {// First line of the error is: // '(line 1, column 5)'
              reason.replace(/line \d, /, '')}
            </Pre>
          </p>
        </Flex>
      ),
      FetchError: msg => <span>{msg}</span>,
      UnknownError: () => <span>Something went wrong :(</span>,
    })(e),
  success: results =>
    results.length ? (
      results.map(r => <SearchResult key={key(r)} result={r} />)
    ) : (
      <span>no results</span>
    ),
})

type RecordsResult = RemoteData<FormattedFunctionRecord[], SearchError>

interface Props {
  records: RecordsResult
}

const Results: React.SFC<Props> = ({ records }) => (
  <div>{renderData(records)}</div>
)

export default Results
