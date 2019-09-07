export type Alias = string

export type Union = string | number

export type Keyed = { [x: string]: Union }

export enum Enum {
  foo = 'foo',
  bar = 'bar',
}

export enum EnumWithDefaults {
  foo,
  bar,
}

export interface Interface {
  prop: string
}
