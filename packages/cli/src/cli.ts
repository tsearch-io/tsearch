import { writeFileSync } from 'fs'
import { promisify } from 'util'
import path from 'path'
import { homedir } from 'os'
import * as yargs from 'yargs'

import _mkdirp from 'mkdirp'
import { ts } from 'ts-simple-ast'
import extractFunctions from 'ts-earch-extract'

const mkdirp = promisify(_mkdirp)

function main() {
  const { _: fileNames, stdout } = yargs.option('stdout', {
    default: false,
    type: 'boolean',
    describe: 'write output to stdout',
  }).argv

  if (fileNames.length === 0) {
    console.error('Missing file path/s')
    process.exit(1)
  }

  const results = extractFunctions(fileNames, {
    compilerOptions: {
      target: ts.ScriptTarget.ES5,
      module: ts.ModuleKind.CommonJS,
    },
  })

  if (stdout) {
    console.log(JSON.stringify(results))
    process.exit(0)
  }

  // TODO: following unix filosofy, stdout should be enough

  const $home = homedir()
  const dir = path.join($home, '.ts-earch')

  mkdirp(dir)
    .then(() => {
      const filePath = path.join(dir, 'types.json')

      writeFileSync(filePath, JSON.stringify(results, null, 2), 'utf-8')

      console.log(`Types writen to ${filePath}`)
    })
    .catch(err => {
      console.error(err)
    })
}

module.exports = main
