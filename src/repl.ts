import * as readline from 'readline';
import { Scanner } from './FormulaParser/scanner';
import { Parser } from './FormulaParser/parsers'
import { Interpreter } from './FormulaParser/interpreter';


const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});


function promptUser() {
  rl.question('> ', (input) => {
    if (input.toLowerCase() === 'exit') {
      rl.close();
      return;
    }
    let scanner = new Scanner(input);
    scanner.scanTokens()
    let tokens = scanner.getTokens()
    // scanner.printTokens()
    let parser = new Parser(tokens)
    let expression = parser.parse()
    // console.log(expression)
    let interpreter  = new Interpreter()

    let result  = interpreter.interpret(expression)
    console.log(result?.toString())
    promptUser(); // Prompt the user again
  });
}


console.log('REPL started. Type "exit" to end.');
promptUser()