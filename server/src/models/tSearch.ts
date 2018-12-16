// TODO: shared code
export interface Param {
  name?: string
  type: string
}

export interface FunctionRecord {
  name?: string
  parameters: Param[]
  returnType: string
  location: {
    path: string
    lines: {
      from: number
      to: number
    }
  }
}
