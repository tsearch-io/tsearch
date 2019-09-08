import * as React from 'react'
import { RemoteData, cata } from 'remote-data-ts'

import SearchResult from '../components/SearchResult'
import { FormattedFunctionRecord } from '../types'

const key = ({ location }: FormattedFunctionRecord) =>
  `${location.path}-${location.lines.from}-${location.lines.to}`

const renderData = cata<FormattedFunctionRecord[], string, React.ReactNode>({
  notAsked: () => <span>Search for functions by name, types or signature</span>,
  loading: () => <span>loading ...</span>,
  failure: e => <span>{e}</span>,
  success: results =>
    results.length ? (
      results.map(r => <SearchResult key={key(r)} result={r} />)
    ) : (
      <span>no results</span>
    ),
})

interface Props {
  records: RemoteData<FormattedFunctionRecord[], string>
}

const Results: React.SFC<Props> = ({ records }) => (
  <div>{renderData(records)}</div>
)

export default Results
