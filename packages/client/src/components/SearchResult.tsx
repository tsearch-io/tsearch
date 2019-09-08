import * as React from 'react'
import styled from 'styled-components'
import SyntaxHighlighter from 'react-syntax-highlighter'
import {
  gruvboxLight,
  gruvboxDark,
} from 'react-syntax-highlighter/dist/styles/hljs'

import Collapse from './Collapse'
import { FormattedFunctionRecord } from '../types'

interface Props {
  result: FormattedFunctionRecord
}

const Container = styled.div({
  marginBottom: 30,
})

const Location = styled.div({
  fontSize: 16,
  display: 'flex',
})

const Toggle = styled.div({
  cursor: 'pointer',
  marginLeft: 20,
})

const codeStyle = {
  padding: 10,
  fontSize: 18,
}
const signatureStyle = {
  ...codeStyle,
  backgroundColor: '#eee',
  marginBottom: 10,
}

const Signature: React.SFC<FormattedFunctionRecord> = props => (
  <SyntaxHighlighter
    language="typescript"
    style={gruvboxLight}
    customStyle={signatureStyle}
  >
    {props.formattedSignature}
  </SyntaxHighlighter>
)

const DTLink: React.SFC<{ module: string }> = ({ module }) => (
  <div>
    Package:{' '}
    <a href={`https://www.npmjs.com/package/@types/${module}`} target="_blank">
      @types/{module}
    </a>
  </div>
)

const SearchResult: React.SFC<Props> = ({ result }) => (
  <Container>
    <Signature {...result} />
    {result.text && (
      <Collapse
        trigger={({ toggle, isOpen }) => (
          <Location>
            <DTLink module={result.module} />
            <Toggle onClick={toggle}>{isOpen ? 'Source ⮟' : 'Source ⮞'}</Toggle>
          </Location>
        )}
      >
        <SyntaxHighlighter
          language="typescript"
          style={gruvboxDark}
          customStyle={codeStyle}
          showLineNumbers={true}
          startingLineNumber={result.location.lines.from}
        >
          {result.text.trim()}
        </SyntaxHighlighter>
      </Collapse>
    )}
  </Container>
)

export default SearchResult
