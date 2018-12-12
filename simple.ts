import {Project, ProjectOptions, ts} from 'ts-simple-ast';

// import {writeFileSync} from 'fs';

interface FunctionRecord {
  name?: string;
  parameters: string[];
  returnType: string;
  location: {
    path: string;
    lines: {
      from: number;
      to: number;
    };
  };
}

function indexFunctions(
  fileNames: string[],
  options: ProjectOptions,
): FunctionRecord[] {
  const project = new Project({
    ...options,
    addFilesFromTsConfig: false,
    skipFileDependencyResolution: true,
  });

  project.addExistingSourceFiles(fileNames);

  return project.getSourceFiles()
    .map(file =>
      file.getFunctions().map(fn => ({
        name: fn.getName(),
        parameters: fn.getParameters().map(param => param.getText()),
        returnType: fn.getReturnType().getText(),
        location: {
          path: file.getFilePath(),
          lines: {
            from: fn.getStartLineNumber(),
            to: fn.getEndLineNumber(),
          },
        },
      })),
    )
    .reduce((acc, cur) => {
      acc.push(...cur)
      return acc
    }, []);
}

const fileNames = process.argv.slice(2);

const results = indexFunctions(fileNames, {
  compilerOptions: {
    target: ts.ScriptTarget.ES5,
    module: ts.ModuleKind.CommonJS,
  },
});

console.log(JSON.stringify(results, null, 2))
