import {
  Project,
  ProjectOptions,
  ts,
  TypeGuards,
  FunctionDeclaration,
  VariableDeclaration,
  ParameterDeclaration,
} from 'ts-simple-ast'

import { FunctionRecord } from 'ts-earch-types'

export default function findFunctions(
  fileNames: string[],
  options: ProjectOptions,
): FunctionRecord[] {
  const project = new Project({
    ...options,
    addFilesFromTsConfig: false,
    skipFileDependencyResolution: true,
  })

  project.addExistingSourceFiles(fileNames)

  return project
    .getSourceFiles()
    .map(file => {
      const path = dtPath(file.getFilePath())
      const module = dtModuleName(path)

      return file.getExportedDeclarations().map(exp => {
        if (TypeGuards.isFunctionDeclaration(exp)) {
          return functionDeclaration(exp, path, module)
        }

        if (TypeGuards.isVariableDeclaration(exp)) {
          return arrowFunction(exp, path, module)
        }

        return undefined
      })
    })
    .reduce((acc, cur) => {
      acc.push(...cur)
      return acc
    }, [])
    .filter((v): v is FunctionRecord => Boolean(v))
}

function functionDeclaration(
  node: FunctionDeclaration,
  path: string,
  module: string,
): FunctionRecord {
  return {
    name: node.getName(),
    text: node.getFullText(),
    // docs: node
    //   .getJsDocs()
    //   .map(doc => doc.getFullText())
    //   .join(''),
    parameters: node.getParameters().map(parameterDeclaration),
    returnType: node.getReturnType().getText(),
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
  node: VariableDeclaration,
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
    // docs: arrow
    //   .getJsDocs()
    //   .map(doc => doc.getFullText())
    //   .join(''),
    parameters: arrow.getParameters().map(parameterDeclaration),
    returnType: arrow.getReturnType().getText(),
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

function parameterDeclaration(param: ParameterDeclaration, index: number) {
  let name
  try {
    name = param.getName() || `t${index + 1}`
  } catch (e) {
    name = `t${index + 1}`
  }

  return {
    name,
    type: param.getType().getText(),
  }
}

function dtModuleName(path: string) {
  return path.split('/')[0]
}

function dtPath(path: string) {
  return path.replace(/^.*DefinitelyTyped\/types\//, '')
}
