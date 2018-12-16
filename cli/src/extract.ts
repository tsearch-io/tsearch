import {
  Project,
  ProjectOptions,
  ts,
  TypeGuards,
  FunctionDeclaration,
  VariableDeclaration,
  ParameterDeclaration,
} from 'ts-simple-ast'

interface Param {
  name?: string
  type: string
}

interface FunctionRecord {
  name?: string
  parameters: Param[]
  returnType: string
  location: {
    path: string
    lines: {
      from: number
      to: number
    }
  }
}

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
      const path = file.getFilePath()

      return file.getExportedDeclarations().map(exp => {
        if (TypeGuards.isFunctionDeclaration(exp)) {
          return functionDeclaration(exp, path)
        }

        if (TypeGuards.isVariableDeclaration(exp)) {
          return arrowFunction(exp, path)
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
): FunctionRecord {
  return {
    name: node.getName(),
    parameters: node.getParameters().map(parameterDeclaration),
    returnType: node.getReturnType().getText(),
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
): FunctionRecord | undefined {
  const [arrow] = node.getChildrenOfKind(ts.SyntaxKind.ArrowFunction)

  if (!arrow) {
    return undefined
  }

  return {
    name: node.getName(),
    parameters: arrow.getParameters().map(parameterDeclaration),
    returnType: arrow.getReturnType().getText(),
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
    name = param.getName() || `param${index + 1}`
  } catch (e) {
    name = `t${index + 1}`
  }

  return {
    name,
    type: param.getType().getText(),
  }
}
