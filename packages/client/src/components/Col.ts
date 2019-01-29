import styled from 'styled-components'

interface Props {
  w: number
}

const Col = styled.div<Props>(({ w }) => ({
  width: `${w}%`,
}))

export default Col
