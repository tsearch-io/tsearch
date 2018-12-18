export interface Param {
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
  name?: string
  parameters: Param[]
  returnType: string
  location: Location
}
