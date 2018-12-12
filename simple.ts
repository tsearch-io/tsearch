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

function generateDocumentation(
  fileNames: string[],
  options: ProjectOptions,
): void {
  const project = new Project({
    ...options,
    addFilesFromTsConfig: false,
    skipFileDependencyResolution: true,
  });

  project.addExistingSourceFiles('test/**/*.ts');

  const files = project.getSourceFiles();

  const results: FunctionRecord[] = files
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
    .reduce((acc, cur) => acc.concat(...cur), []);

  console.log(JSON.stringify(results, null, 2));
}

const files = process.argv.slice(2);

generateDocumentation(files, {
  compilerOptions: {
    target: ts.ScriptTarget.ES5,
    module: ts.ModuleKind.CommonJS,
  },
});
