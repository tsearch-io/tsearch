export interface Parameter {
  name: string
  type: string
}

export interface Location {
  path: string
  lines: {
    from: number
    to: number
  }
}

export interface FunctionRecord {
  name: string
  parameters: Parameter[]
  returnType: string
  location: Location
}
