import React, {FC, useState} from 'react'

interface Props {
  trigger: (p: {toggle: () => void; isOpen: boolean}) => React.ReactNode
}

const Collapse: FC<Props> = ({trigger, children}) => {
  const [isOpen, setIsOpen] = useState(false)

  const onToggle = () => setIsOpen(s => !s)

  return (
    <React.Fragment>
      {trigger({isOpen, toggle: onToggle})}
      {isOpen && children}
    </React.Fragment>
  )
}

export default Collapse
