import * as React from 'react'
import styled from 'styled-components'
import SyntaxHighlighter from 'react-syntax-highlighter'
import {
  gruvboxLight,
  gruvboxDark,
} from 'react-syntax-highlighter/dist/esm/styles/hljs'

import Collapse from './Collapse'
import {FormattedFunctionRecord} from '../types'

interface Props {
  result: FormattedFunctionRecord
}

const Container = styled.div({
  marginBottom: 50,
})

const Location = styled.div({
  fontSize: 16,
  verticalAlign: 'center',
})

const Toggle = styled.span({
  cursor: 'pointer',
  marginLeft: 20,
})

const codeStyle = {
  padding: 10,
  fontSize: 16,
}
const signatureStyle = {
  ...codeStyle,
  backgroundColor: 'rgba(0,0,0,.03)',
  marginBottom: 10,
  marginTop: 10,
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

const PackageIcon = styled.div({
  display: 'inline-flex',
  justifyContent: 'center',
  alignItems: 'center',
  backgroundColor: 'rgb(120, 33, 117)',
  color: 'white',
  borderRadius: '100%',
  fontSize: 14,
  fontWeight: 'bold',
  lineHeight: 1,
  textAlign: 'center',
  width: 20,
  height: 20,
})

const ModuleLink: React.FC<{module: string}> = ({module}) => (
  <>
    <PackageIcon>P</PackageIcon>{' '}
    <a
      href={`https://www.npmjs.com/package/${module}`}
      target="_blank"
      rel="noopener noreferrer"
    >
      {module}
    </a>
  </>
)

const Name = styled.div({
  fontWeight: 'bold',
  fontSize: 20,
})

const SearchResult: React.SFC<Props> = ({result}) => (
  <Container>
    <Name>{result.name || 'anonymous'}</Name>
    <Signature {...result} />
    {result.text ? (
      <Collapse
        trigger={({toggle, isOpen}) => (
          <Location>
            <ModuleLink module={result.module} />
            <Toggle onClick={toggle}>
              {isOpen ? '\u2b07\ufe0f Hide Source' : '\u27a1\ufe0f Show Source'}
            </Toggle>
          </Location>
        )}
      >
        <SyntaxHighlighter
          language="typescript"
          style={gruvboxDark}
          customStyle={codeStyle}
          showLineNumbers={false}
        >
          {result.text.trim()}
        </SyntaxHighlighter>
      </Collapse>
    ) : (
      <ModuleLink module={result.module} />
    )}
  </Container>
)

export default SearchResult
