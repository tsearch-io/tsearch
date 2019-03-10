export interface Param {
  name: string
  type: string
  isGeneric: boolean
}

export interface Location {
  path: string
  lines: {
    from: number
    to: number
  }
}

export interface FunctionRecord {
  name?: string
  docs?: string
  text?: string
  parameters: Param[]
  returnType: string
  location: Location
  module: string
}

export interface Module {
  name: string
  fns: FunctionRecord[]
}
