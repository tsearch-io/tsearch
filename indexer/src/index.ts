import path from 'path'

import * as ts from 'ts-morph'

import {FunctionRecord, callSignature} from './types'

export default function findFunctions(
  directory: string,
  options: ts.ProjectOptions,
): FunctionRecord[] {
  const project = new ts.Project({
    ...options,
    addFilesFromTsConfig: false,
    skipFileDependencyResolution: true,
  })

  const filesGlob = path.join(directory, '**', '*.ts')
  let packageJson
  try {
    packageJson = require(path.join(directory, 'package.json'))
  } catch (e) {
    packageJson = {}
  }

  const moduleName =
    packageJson?.name ?? path.relative(path.join(directory, '..'), directory)

  project.addSourceFilesAtPaths(filesGlob)

  return project.getSourceFiles().reduce<FunctionRecord[]>((acc, file) => {
    const fileName = path.relative(directory, file.getFilePath())

    const entries = Array.from(file.getExportedDeclarations().entries())

    const fns = entries.reduce<FunctionRecord[]>(
      (acc, [_indentifier, exports]) => {
        exports.forEach(exp => {
          if (ts.TypeGuards.isFunctionDeclaration(exp)) {
            acc.push(functionDeclaration(exp, fileName, moduleName))
            return
          }

          if (ts.TypeGuards.isVariableDeclaration(exp)) {
            const fn = arrowFunction(exp, fileName, moduleName)
            if (fn) {
              acc.push(fn)
            }
          }
        })

        return acc
      },
      [],
    )

    acc.push(...fns)
    return acc
  }, [])
}

function functionDeclaration(
  node: ts.FunctionDeclaration,
  file: string,
  module: string,
): FunctionRecord {
  return {
    name: node.getName(),
    text: node.getFullText(),
    docs: node
      .getJsDocs()
      .map(doc => doc.getFullText())
      .join(''),
    signature: callSignature({
      typeParameters: node.getTypeParameters(),
      parameters: node.getParameters(),
      returnType: node.getReturnType(),
    }),
    module,
    location: {
      path: file,
      lines: {
        from: node.getStartLineNumber(),
        to: node.getEndLineNumber(),
      },
    },
  }
}

function arrowFunction(
  node: ts.VariableDeclaration,
  file: string,
  module: string,
): FunctionRecord | undefined {
  const [arrow] = node.getChildrenOfKind(ts.SyntaxKind.ArrowFunction)

  if (!arrow) {
    return undefined
  }

  return {
    name: node.getName(),
    text: `const${node.getFullText()}`,
    docs: arrow
      .getJsDocs()
      .map(doc => doc.getFullText())
      .join(''),
    signature: callSignature({
      typeParameters: arrow.getTypeParameters(),
      parameters: arrow.getParameters(),
      returnType: arrow.getReturnType(),
    }),
    module,
    location: {
      path: file,
      lines: {
        from: node.getStartLineNumber(),
        to: node.getEndLineNumber(),
      },
    },
  }
}
