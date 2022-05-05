import styled from 'styled-components'

const none = {outline: 'none'}

const Input = styled.input({
  outline: 'none',
  padding: 5,
  border: '1px solid black',
  marginBottom: 10,
  marginRight: 10,
  fontSize: 18,
  width: 400,
  maxWidth: '90vw',
  '&:hover': none,
  '&:active': none,
  '&:focus': none,
})

export default Input
