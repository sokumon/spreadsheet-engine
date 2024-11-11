import * as ts from 'typescript';
import * as path from "path"
const fs = require('node:fs');

function extractFunctionDefinitions(file: string) {
    let program = ts.createProgram([file], { allowJs: true });
    const sourceFile = program.getSourceFile(file);

    if (!sourceFile) {
        console.error(`Could not find source file: ${filePath}`);
        return;
    }

    let completeions: Array<Object> = []
    ts.forEachChild(sourceFile, node => {
       
        if (ts.isFunctionDeclaration(node)) {
            let name = node.name.escapedText

            const parameters = node.parameters.map(param => ({
            name: param.name.getText(sourceFile),
            type: param.type ? param.type.getText(sourceFile) : 'any',
            }));

            const returnType = node.type ? node.type.getText(sourceFile) : 'void';

            completeions.push({
                name,
                parameters,
                returnType
            });

        }
      });

    return completeions
}

// Example usage
const filePath = path.resolve(__dirname, '../../node_modules/@formulajs/formulajs/types/cjs/index.d.cts');// Replace with your actual path
// const filePath = 'test.d.cts'
const functions = extractFunctionDefinitions(filePath);
let content = JSON.stringify(functions,null,3)
fs.writeFile('completion.json', content, err => {
    if (err) {
      console.error(err);
    } else {
      // file written successfully
    }
  });