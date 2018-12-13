import {writeFileSync} from 'fs';
import path from 'path';

import {ts} from 'ts-simple-ast';

import extractFunctions from './extract';

const fileNames = process.argv.slice(2);

const results = extractFunctions(fileNames, {
  compilerOptions: {
    target: ts.ScriptTarget.ES5,
    module: ts.ModuleKind.CommonJS,
  },
});

const filePath = path.join(__dirname, '../functions.json')

writeFileSync(filePath, JSON.stringify(results, null, 2), 'utf-8');
