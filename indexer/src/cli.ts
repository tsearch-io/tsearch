import {writeFileSync} from 'fs'
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

const main = () => {
  const {_: directories, out} = cli.argv

  const results: {
    name: string
    type: TsMorph.Structure
  }[] = directories.flatMap(directory =>
    Tsearch.findFunctions(directory, {
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

  writeFileSync(out, JSON.stringify(results, null, 2), 'utf-8')

  console.log(`Types writen to ${out}`)
}

module.exports = main
