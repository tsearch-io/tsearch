import * as ts from 'ts-morph'

import { FunctionRecord } from 'ts-earch-types'

import { callSignature } from './type'

export default function findFunctions(
  fileNames: string[],
  options: ts.ProjectOptions,
): FunctionRecord[] {
  const project = new ts.Project({
    ...options,
    addFilesFromTsConfig: false,
    skipFileDependencyResolution: true,
  })

  project.addExistingSourceFiles(fileNames)

  return project.getSourceFiles().reduce<FunctionRecord[]>((acc, file) => {
    const path = dtPath(file.getFilePath())
    const module = dtModuleName(path)

    const entries = Array.from(file.getExportedDeclarations().entries())

    const fns = entries.reduce<FunctionRecord[]>(
      (acc, [indentifier, exports]) => {
        exports.forEach(exp => {
          if (ts.TypeGuards.isFunctionDeclaration(exp)) {
            acc.push(functionDeclaration(exp, path, module))
            return
          }

          if (ts.TypeGuards.isVariableDeclaration(exp)) {
            const fn = arrowFunction(exp, path, module)
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
  path: string,
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
      path,
      lines: {
        from: node.getStartLineNumber(),
        to: node.getEndLineNumber(),
      },
    },
  }
}

function arrowFunction(
  node: ts.VariableDeclaration,
  path: string,
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
      path,
      lines: {
        from: node.getStartLineNumber(),
        to: node.getEndLineNumber(),
      },
    },
  }
}

function dtModuleName(path: string) {
  return path.split('/')[0]
}

function dtPath(path: string) {
  return path.replace(/^.*DefinitelyTyped\/types\//, '')
}
