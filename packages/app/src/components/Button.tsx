import styled from 'styled-components'

const Button = styled.button`
  outline: none;

  padding: 5px;
  border: 1px solid black;
  margin-bottom: 5px;

  cursor: pointer;

  &:hover,
  &:active,
  &:focus {
    outline: none;
  }

  &:disabled {
    cursor: not-allowed;
  }
`

export default Button
