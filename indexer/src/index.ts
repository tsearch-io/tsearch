import path from 'path'

import * as ts from 'ts-morph'

export const findFunctions = (
  directory: string,
  options: ts.ProjectOptions,
): {name: string; type: ts.Structure}[] => {
  const project = new ts.Project({
    ...options,
    addFilesFromTsConfig: false,
    skipFileDependencyResolution: true,
  })

  const filesGlob = path.join(directory, '**', '*.ts')

  project.addSourceFilesAtPaths(filesGlob)

  const sourceFiles = project.getSourceFiles()
  const decls: {name: string; type: ts.Structure}[] = sourceFiles.flatMap(x =>
    Array.from(x.getExportedDeclarations().values()).flatMap(decls =>
      decls.flatMap(decl => {
        // ((decl as any).getTypeNode() as any).getStructure(),
        if (!('getTypeNode' in decl)) return []
        const typeNode = decl.getTypeNode()
        if (typeNode === undefined || !('getStructure' in typeNode)) return []
        const node = {
          name: decl.getName(),
          type: (typeNode as any).getStructure(),
        }
        return [node]
      }),
    ),
  )
  return decls
}
