import { Value } from "./FormulaParser/defination";
import { TokenTypes } from "./FormulaParser/defination";
import { Scanner } from "./FormulaParser/";
import { CellType } from "./FormulaParser/defination";
class Cell{
    rawFormulaText: string = null;
    value: Value;
    type: CellType;
    dependency: Array<string>;

    constructor(){
        this.dependency = [];
    }
    assignType(){
        let onlyNumberRegex = /\d*/
        if(this.value?.toString().match(onlyNumberRegex)![0] != ""){
            this.type = CellType.Number
        }else{
            this.type = CellType.String
        }
    }

    trackDependency(){
        // just tracking cells right now not recalucating it
        let scanner = new Scanner(this.rawFormulaText)
        scanner.scanTokens()
        let tokens = scanner.getTokens()
        let cells = tokens.filter(token => token.type == TokenTypes.Cell )
        // this.dependency.push(cells)
        cells.forEach(cell => {
            console.log(cell)
            this.dependency.push(cell.value)
        });
    }
}

export { Cell }