import * as React from 'react'
import styled from 'styled-components'

import { Parameter, FunctionRecord } from '../models/tSearch'

interface Props {
  result: FunctionRecord
}

const signature = (parameters: Parameter[], returnType: string) =>
  `(${parameters
    .map(({ name, type }) => `${name}: ${type}`)
    .join(', ')}) => ${returnType}`

const Container = styled.div`
  margin-bottom: 20px;
`

const SignatureContainer = styled.div`
  font-family: monospace;
  font-size: 18px;

  margin-bottom: 5px;
  padding: 5px 3px;

  background-color: #ccc;
  border-top: 1px solid grey;
`

const Location = styled.div`
  font-size: 14px;
`

const Red = styled.span`
  color: #721c24;
`

const Signature: React.SFC<FunctionRecord> = ({
  name,
  parameters,
  returnType,
}) => (
  <SignatureContainer>
    <Red>{name}</Red> :: {signature(parameters, returnType)}
  </SignatureContainer>
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
