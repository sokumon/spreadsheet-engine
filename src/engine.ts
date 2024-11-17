import { formulas } from "./FormulaParser/formula";
import {argSuggestions } from "./completion.js"
import { Workbook } from "./workbook";
class Engine {

    formulas: Object;
    // These are arg completions ideally shoudl be in formulas itself
    argSuggestions: Object;
    workbook: Workbook;
    constructor() {
        this.formulas = formulas
        this.argSuggestions = argSuggestions
    }
    
    createWorkbook(title: string){
        this.workbook = new Workbook(title)
        return this.workbook
    }

    saveWorkbook(){
        // let workBookObj = {
        //     title : this.workbook.title,
        //     sheets: this.workbook.save()
        // }
        let workBookObj = this.workbook
        workBookObj.sheets = workBookObj.removeDataKey()
        return JSON.stringify(workBookObj)
    }

    loadWorkbook(workbookString: string){
        let workbookData = JSON.parse(workbookString)
        this.workbook = new Workbook(workbookData.title)
        this.workbook.sheets = workbookData.sheets 
        return this.workbook
    }

    renameWorkbook(wbName: string){
        this.workbook.title = wbName
    }
    
    
}

export default Engine;
