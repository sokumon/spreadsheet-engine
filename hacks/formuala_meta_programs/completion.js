"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts = require("typescript");
var path = require("path");
var fs = require('node:fs');
function extractFunctionDefinitions(file) {
    var program = ts.createProgram([file], { allowJs: true });
    var sourceFile = program.getSourceFile(file);
    if (!sourceFile) {
        console.error("Could not find source file: ".concat(filePath));
        return;
    }
    var completeions = [];
    ts.forEachChild(sourceFile, function (node) {
        if (ts.isFunctionDeclaration(node)) {
            var name_1 = node.name.escapedText;
            var parameters = node.parameters.map(function (param) { return ({
                name: param.name.getText(sourceFile),
                type: param.type ? param.type.getText(sourceFile) : 'any',
            }); });
            var returnType = node.type ? node.type.getText(sourceFile) : 'void';
            completeions.push({
                name: name_1,
                parameters: parameters,
                returnType: returnType
            });
        }
    });
    return completeions;
}
// Example usage
var filePath = path.resolve(__dirname, '../../node_modules/@formulajs/formulajs/types/cjs/index.d.cts'); // Replace with your actual path
// const filePath = 'test.d.cts'
var functions = extractFunctionDefinitions(filePath);
var content = JSON.stringify(functions, null, 3);
fs.writeFile('completion.json', content, function (err) {
    if (err) {
        console.error(err);
    }
    else {
        // file written successfully
    }
});
