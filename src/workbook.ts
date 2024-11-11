import { Sheet } from "./sheet"

export class WorkBook{
    // this and the title of the sheet should match
    title: string;
    sheets: Sheet[]

    constructor(title: string) {
        this.title = title,
        this.sheets = []
    }
    addSheet(): Sheet{
        let name = `Sheet ${this.sheets.length + 1}`
        let newSheet = new Sheet(name)
        this.sheets.push(newSheet)
        return newSheet
    }

    renameSheet(oldName: string, newName: string): boolean{
        let sheetExists = this.sheets.find(sheet => sheet.name == oldName)
        if(sheetExists){
            sheetExists.name = newName
            return true
        }
        return false
    }

    getSheetNames(): string[]{
        let sheetNames: string[]= []
        for(let i=0; i < this.sheets.length ;i++){
            sheetNames.push(this.sheets[i].name)
        }
        return sheetNames
    }

    save(){
        return JSON.stringify(this.sheets)
    }
}