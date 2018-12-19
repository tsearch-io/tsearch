import * as React from 'react'

interface Props {
  trigger: (p: { toggle: () => void; isOpen: boolean }) => React.ReactNode
}

interface State {
  isOpen: boolean
}

export default class Collapse extends React.Component<Props, State> {
  state = { isOpen: false }

  onToggle = () => this.setState(s => ({ isOpen: !s.isOpen }))

  render() {
    const { isOpen } = this.state
    return (
      <React.Fragment>
        {this.props.trigger({ isOpen, toggle: this.onToggle })}
        {isOpen && this.props.children}
      </React.Fragment>
    )
  }
}
