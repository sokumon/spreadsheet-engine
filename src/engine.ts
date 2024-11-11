import { formulas } from "./FormulaParser/formula";
import {argSuggestions } from "./completion.js"
import { WorkBook } from "./workbook";
class Engine {

    formulas: Object;
    // These are arg completions ideally shoudl be in formulas itself
    argSuggestions: Object;
    constructor() {
        this.formulas = formulas
        this.argSuggestions = argSuggestions
    }

    createWorkBook(title: string){
        return new WorkBook(title)
    }
    // Method to parse the formula


    
    
}

export default Engine;
