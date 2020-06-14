import styled from 'styled-components'

interface Props {
  w: number
}

const Col = styled.div<Props>(({w}) => ({
  width: `${w}%`,
  '@media screen and (max-width: 1024px)': {
    width: '100%',
  },
}))

export default Col
