import { writeFileSync } from 'fs'
import path from 'path'
import { homedir } from 'os'
import * as yargs from 'yargs'

import { ts } from 'ts-morph'
import extractFunctions from 'ts-earch-extract'

const cli = yargs.option('out', {
  // TODO: figure out how to show the description on --help
  type: 'string',
  description: 'Out file path. If missing will write to stdout',
  hidden: false,
})

function main() {
  const { _: fileNames, out } = cli.argv

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

  if (!out) {
    console.log(JSON.stringify(results))
    return
  }

  const filePath = path.normalize(out.replace(/^~/, homedir()))

  writeFileSync(filePath, JSON.stringify(results, null, 2), 'utf-8')

  console.log(`Types writen to ${filePath}`)
}

module.exports = main
