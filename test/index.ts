export function reverse(a: string) {
  return a
    .split('')
    .reverse()
    .join('')
}

export function split(a: string, param: string) {
  return a.split(param)
}

export const add = (a: number, b: number) => a + b

interface User {
  name: string
  lastName: string
}

export const email = (user: User) =>
  `${user.name}.${user.lastName}@company-x.com`

export const greet = ({ name, lastName }: { name: string; lastName: string }) =>
  `Good morning ${name} ${lastName}`

export const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B) => (a: A): C =>
  f(g(a))
