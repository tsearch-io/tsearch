import {writeFileSync} from 'fs'
import path from 'path'
import {homedir} from 'os'
import * as yargs from 'yargs'
import * as Types from './types'

import {ts} from 'ts-morph'

import extractFunctions from './index'

// .option('directory', {type: 'positional'})

const cli = yargs.option('out', {
  // TODO: figure out how to show the description on --help
  type: 'string',
  description: 'Out file path. If missing will write to stdout',
  hidden: false,
})

const normalize = (p: string) => path.normalize(p.replace(/^~/, homedir()))

function main() {
  const {_: directories, out} = cli.argv

  const results: Types.FunctionRecord[] = directories.flatMap(directory =>
    extractFunctions(normalize(directory), {
      compilerOptions: {
        target: ts.ScriptTarget.ES5,
        module: ts.ModuleKind.CommonJS,
      },
    }),
  )

  if (!out) {
    console.log(JSON.stringify(results))
    return
  }

  const filePath = normalize(out)

  writeFileSync(filePath, JSON.stringify(results, null, 2), 'utf-8')

  console.log(`Types writen to ${filePath}`)
}

module.exports = main
