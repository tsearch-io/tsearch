import * as ts from 'typescript';

import {writeFileSync} from 'fs';

// ----------------------------------

interface FunctionRecord {
  name?: string;
  signature: string;
  documentation?: string;
  parameters: string[];
  returnType: string;
  location: {
    fileName?: string;
    lines: {
      from: number;
      to: number;
    };
  };
}

/** Generate documention for all classes in a set of .ts files */
function generateDocumentation(
  fileNames: string[],
  options: ts.CompilerOptions,
): void {
  // Build a program using the set of root file names in fileNames
  let program = ts.createProgram(fileNames, options);

  // Get the checker, we will use it to find more about classes
  let checker = program.getTypeChecker();

  let output: FunctionRecord[] = [];

  // Visit every sourceFile in the program
  program.getSourceFiles().forEach(sourceFile => {
    // Walk the tree to search for classes
    if (sourceFile.fileName === 'test/index.ts') {
      ts.forEachChild(sourceFile, visit(sourceFile));
    }
  });

  const content = JSON.stringify(output, null, 2);

  console.log(content);

  // print out the doc
  writeFileSync('functions.json', content);

  return;

  /** visit nodes finding exported classes */
  function visit(file: ts.SourceFile) {
    return (node: ts.Node) => {
      // Only consider exported nodes
      if (!isNodeExported(node)) {
        return;
      }

      // Get function types
      if (ts.isFunctionDeclaration(node)) {
        // No need to walk any further, class expressions/inner declarations
        const serialized = serializeFunction(
          file,
          node as ts.FunctionDeclaration,
        );

        serialized && output.push(serialized);
      }
      // This is a namespace, visit its children
      else if (ts.isModuleDeclaration(node)) {
        ts.forEachChild(node, visit(file));
      }
    };
  }

  /** Serialize a symbol into a json object */
  // function serializeSymbol(symbol: ts.Symbol): FunctionRecord {
  //   return {
  //     name: symbol.getName(),
  //     documentation: ts.displayPartsToString(
  //       symbol.getDocumentationComment(undefined)
  //     ),
  //     signature: checker.typeToString(
  //       checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration)
  //     ),
  //     returnType: "",
  //     parameters: [],
  //     location: {
  //       fileName: "",
  //       lines: {
  //         from: 10,
  //         to: 12
  //       }
  //     }
  //   };
  // }

  /** Serialize a function type */
  function serializeFunction(
    file: ts.SourceFile,
    node: ts.FunctionDeclaration,
  ): FunctionRecord {
    // TODO: this is not what I'm looking for - I already have the node, should be enough to get information from it
    // let symbol = checker.getSymbolAtLocation(node);

    // if (symbol) {
    //   let details = serializeSymbol(symbol);

    //   // Get the construct signatures
    //   // if (node.decorators) {
    //   //   details.decorators = node.decorators.map(serializeDecorator).filter(Boolean) as FunctionRecord[];

    //   //   let constructorType = checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration);

    //   //   details.constructors = constructorType.getConstructSignatures().map(serializeSignature);

    //   //   return details;
    //   // }
    //   return details;
    // }
    // return undefined;
    //
    node.forEachChild(node => {
      console.log(node.getText(file))
    })

    // // I'm assuming you know how to get the type checker from the compiler too.
    // const typeChecker = ...;
    // // Navigate through the tree to get to the method declaration located in the class.
    // // This is the node with kind === ts.SyntaxKind.MethodDeclaration.
    // // You will have to assert the node to ts.MethodDeclaration.
    // const methodDeclaration = ... as ts.MethodDeclaration;

    // const signature = typeChecker.getSignatureFromDeclaration(methodDeclaration);
    // const returnType = typeChecker.getReturnTypeOfSignature(signature);
    // const parameters = methodDeclaration.parameters; // array of Parameters
    // const docs = methodDeclaration.jsDoc; // array of js docs

    const signature = checker.getSignatureFromDeclaration(node)

    if (signature) {
      console.log(signature)
      // signature.getReturnType()
    }


    return {
      name: node.name && node.name.text,
      documentation: '',
      signature: (node.type && node.type.getFullText(file)) || '',
      returnType: '',
      parameters: node.parameters.map(param => {
        return param.getText(file)          // all parameter: `a: string`
        // return param.name.getFullText(file) // parameter name `a`
      }),
      location: {
        fileName: '',
        lines: {
          from: 10,
          to: 12,
        },
      },
    };
  }

  // function serializeDecorator(decorator: ts.Decorator) {
  //     const node = decorator.expression.getFirstToken();
  //     if (node) {
  //       let symbol = checker.getSymbolAtLocation(node);
  //       if (symbol) {
  //         let decoratorType = checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration);
  //         let details = serializeSymbol(symbol);
  //         details.constructors = decoratorType.getCallSignatures().map(serializeSignature);
  //         return details;
  //       }
  //     }

  //     return undefined;
  // }

  /** Serialize a signature (call or construct) */
  // function serializeSignature(signature: ts.Signature) {
  //     return {
  //         parameters: signature.parameters.map(serializeSymbol),
  //         returnType: checker.typeToString(signature.getReturnType()),
  //         documentation: ts.displayPartsToString(signature.getDocumentationComment(undefined))
  //     };
  // }

  /** True if this is visible outside this file, false otherwise */
  function isNodeExported(node: ts.Node): boolean {
    // console.log({
    //   flags: node.flags,
    //   nodeFlagsExportContext: ts.NodeFlags.ExportContext,
    //   and: node.flags & ts.NodeFlags.ExportContext,
    //   parentKind: node.parent && node.parent.kind,
    //   syntaxKindSourceFile: ts.SyntaxKind.SourceFile
    // })
    // TODO: this doesn't work
    // it was ts.NodeFlags.Export but it doesn't exist
    // return (
    //   (node.flags & ts.NodeFlags.ExportContext) !== 0 ||
    //   (node.parent && node.parent.kind === ts.SyntaxKind.SourceFile)
    // );
    return true;
  }
}

const files = process.argv.slice(2);

console.log(files);

generateDocumentation(files, {
  target: ts.ScriptTarget.ES5,
  module: ts.ModuleKind.CommonJS,
});
