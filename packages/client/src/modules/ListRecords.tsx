import * as React from 'react'

import { FunctionRecord } from 'ts-earch-types'

import SearchResult from '../components/SearchResult'
import { RemoteData, match } from '../lib/remoteData'

const key = ({ location }: FunctionRecord) =>
  `${location.path}-${location.lines.from}-${location.lines.to}`

const renderData = match<FunctionRecord[], React.ReactNode>({
  notAsked: () => <span>search for function types</span>,
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
  records: RemoteData<FunctionRecord[]>
}

const Results: React.SFC<Props> = ({ records }) => (
  <div>{renderData(records)}</div>
)

export default Results
