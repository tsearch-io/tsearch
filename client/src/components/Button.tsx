import styled from 'styled-components'

const none = {outline: 'none'}

const Button = styled.button({
  outline: 'none',
  padding: 5,
  border: '1px solid black',
  marginBottom: 10,
  marginRight: 10,
  fontSize: 18,
  minWidth: 100,
  cursor: 'pointer',
  '&:hover': none,
  '&:active': none,
  '&:focus': none,
  '&:disabled': {cursor: 'not-allowed'},
})

export default Button
