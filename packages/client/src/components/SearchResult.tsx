import * as React from 'react'
import styled from 'styled-components'
import SyntaxHighlighter from 'react-syntax-highlighter'
import {
  gruvboxLight,
  gruvboxDark,
} from 'react-syntax-highlighter/dist/styles/hljs'

import { Param, FunctionRecord } from 'ts-earch-types'

import Collapse from './Collapse'

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
  display: flex;
`

const Toggle = styled.div`
  cursor: pointer;
  margin-left: 20px;
`

const codeStyle = {
  padding: 10,
  fontSize: 18,
}
const signatureStyle = {
  ...codeStyle,
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
    style={gruvboxLight}
    customStyle={signatureStyle}
  >
    {signature(parameters, returnType, name)}
  </SyntaxHighlighter>
)

const SearchResult: React.SFC<Props> = ({ result }) => (
  <Container>
    <Signature {...result} />
    {result.text && (
      <Collapse
        trigger={({ toggle, isOpen }) => (
          <Location>
            {/* TODO: open $EDITOR or file */}
            <a href="#">
              {result.location.path} ({result.location.lines.from}-
              {result.location.lines.to})
            </a>
            <Toggle onClick={toggle}>{isOpen ? 'Source ⮟' : 'Source ⮞'}</Toggle>
          </Location>
        )}
      >
        <SyntaxHighlighter
          language="typescript"
          style={gruvboxDark}
          customStyle={codeStyle}
          showLineNumbers={true}
        >
          {result.text.trim()}
        </SyntaxHighlighter>
      </Collapse>
    )}
  </Container>
)

export default SearchResult
