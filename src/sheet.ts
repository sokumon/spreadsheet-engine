import { Scanner, Parser, Interpreter } from "./FormulaParser/";
import { Cell } from "./cell";
// index signature as dont know what all styles can be added
// https://www.typescriptlang.org/docs/handbook/2/objects.html#index-signatures
interface CellStyleInterface {
    [key: string]: { [property: string]: string | number }; 
}
interface SheetStyleInterface { [key: string| number]: string | number }
class Sheet{
    name: string;
    cells: Record<string,Cell>;
    data = new Array();
    cellStyles: CellStyleInterface
    sheetStyles: SheetStyleInterface;



    constructor(name: string = "Untitiled Sheet") {
        this.name = name;
        this.cells = {}
        this.data = []
        this.cellStyles = {}
        this.sheetStyles = {}
    }

    parseFormula(input: string): any {
        const scanner = new Scanner(input);
        scanner.scanTokens();
        const tokens = scanner.getTokens();
        const parser = new Parser(tokens);
        parser.cells = this.cells
        const expression = parser.parse();
        const interpreter =  new Interpreter()
        const result = interpreter.interpret(expression);
        return result; 
    }



    addCell(key: string, value: string){
        let cell = new Cell()
        if (value.charAt(0) == "="){
            value = value.replace("=","")
            cell.rawFormulaText = value
            cell.trackDependency()
        }else{
            cell.value = value
            cell.assignType()

        }
        this.cells[key] = cell
        
    }

    getCell(key:string){
        return this.cells[key]
    }
    recalculate(key: string){
        let cell = this.cells[key];
        if (cell.rawFormulaText){
            cell.value = this.parseFormula(cell.rawFormulaText.replace("=",""))
        }
    }
    
    initSheet(){
        for (let j = 0; j < 1000; j++) {
              let row = [];
              for (let n = 0; n < 26; n++) {
                row.push("");
              }
              this.data.push(row);
            }
        return this.data
    }

    flush() {
        if (this.cells != null) {
        for (const [key, cell] of Object.entries(this.cells)) {
            console.log(key,cell)
            let indexes = key.match(/[a-zA-Z]+|[0-9]+/g);
            if (indexes && indexes.length > 0) {
                let colIndex = indexes[0].charCodeAt(0) - 65;
                let rowIndex = parseInt(indexes[1]) - 1;
                this.recalculate(key)
                this.data[rowIndex][colIndex] = cell.value
            } else {
                console.error("No valid indexes found in the key.");
            }
        }
        }
        return this.data
    }
    
    addtoLocalStorage(): any{
        localStorage.setItem(this.name,JSON.stringify(this.cells))
    }

    scan(line:string){
        const scanner = new Scanner(line);
        scanner.scanTokens()
        return scanner.getTokens();
    }

    // StyleObject will be { "A1" : }
    addStyles(type:string, styleObject: CellStyleInterface| SheetStyleInterface){
        let targetStyles = type === "cell" ? this.cellStyles : this.sheetStyles;
    
        for (let key in styleObject) {
            console.log(styleObject[key])
            if (targetStyles[key]) {
                Object.assign(targetStyles[key], styleObject[key]);
            } else {
                targetStyles[key] = styleObject[key];
            }
        }
    }


}
export {
    Sheet
}