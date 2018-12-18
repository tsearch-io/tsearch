import * as React from 'react'
import styled from 'styled-components'
import SyntaxHighlighter from 'react-syntax-highlighter'
import { gruvboxLight as style } from 'react-syntax-highlighter/dist/styles/hljs'

import { Param, FunctionRecord } from 'ts-earch-types'

interface Props {
  result: FunctionRecord
}

// https://github.com/conorhastings/react-syntax-highlighter
const signature = (parameters: Param[], returnType: string, fnName?: string) =>
  `${fnName || 'func'} :: (${parameters
    .map(({ name, type }) => `${name}: ${type}`)
    .join(', ')}) => ${returnType}`

const Container = styled.div`
  margin-bottom: 30px;
`

const Location = styled.div`
  font-size: 16px;
`

const signatureStyle = {
  padding: 10,
  fontSize: 18,
  backgroundColor: '#eee',
  marginBottom: 10,
}

const Signature: React.SFC<FunctionRecord> = ({
  name,
  parameters,
  returnType,
}) => (
  <SyntaxHighlighter
    language="typescript"
    style={style}
    customStyle={signatureStyle}
  >
    {signature(parameters, returnType, name)}
  </SyntaxHighlighter>
)

const SearchResult: React.SFC<Props> = ({ result }) => (
  <Container>
    <Signature {...result} />
    <Location>
      {/* TODO: open $EDITOR or file */}
      <a href="#">
        {result.location.path} ({result.location.lines.from}-
        {result.location.lines.to})
      </a>
    </Location>
  </Container>
)

export default SearchResult
