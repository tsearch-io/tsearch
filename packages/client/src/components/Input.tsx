import styled from 'styled-components'

const Input = styled.input`
  outline: none;

  padding: 5px;
  border: 1px solid black;
  margin-bottom: 5px;
  font-size: 18px;

  width: 400px;

  &:hover,
  &:active,
  &:focus {
    outline: none;
  }
`

export default Input
