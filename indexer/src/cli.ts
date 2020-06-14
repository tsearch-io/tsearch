import {writeFileSync} from 'fs'
import path from 'path'
import {homedir} from 'os'
import * as yargs from 'yargs'

import * as TsMorph from 'ts-morph'

import * as Tsearch from './index'

// .option('directory', {type: 'positional'})

const cli = yargs.option('out', {
  // TODO: figure out how to show the description on --help
  type: 'string',
  description: 'Out file path. If missing will write to stdout',
  hidden: false,
})

const normalize = (p: string) => path.normalize(p.replace(/^~/, homedir()))

const main = () => {
  const {_: directories, out} = cli.argv

  const results: {
    name: string
    type: TsMorph.Structure
  }[] = directories.flatMap(directory =>
    Tsearch.findFunctions(normalize(directory), {
      compilerOptions: {
        target: TsMorph.ts.ScriptTarget.ES5,
        module: TsMorph.ts.ModuleKind.CommonJS,
      },
    }),
  )

  if (!out) {
    process.stdout.write(JSON.stringify(results) + '\n')
    return
  }

  const filePath = normalize(out)

  writeFileSync(filePath, JSON.stringify(results, null, 2), 'utf-8')

  console.log(`Types writen to ${filePath}`)
}

module.exports = main
