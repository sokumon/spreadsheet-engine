import { Sheet } from "./sheet"

export class Workbook{
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
            if(newName in this.getSheetNames()) return false
            sheetExists.name = newName
            return true
        }
        return false
    }

    deleteSheet(name: string){
        let sheetExists = this.sheets.find(sheet => sheet.name == name)
        if(sheetExists){
            let sheetIndex = this.sheets.indexOf(sheetExists)
            this.sheets.splice(sheetIndex,1)
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

    removeDataKey(){
        // removing the 'data' because this is only for working with Datatable for the frontend
        let savedSheets: Sheet[]= []
        this.sheets.forEach(sheet =>{
            let tempSheet = JSON.parse(JSON.stringify(sheet))
            delete tempSheet["data"]
            savedSheets.push(tempSheet)
        })
        return savedSheets
    }

    save(){
        let sheettoSave = this.removeDataKey()
        return JSON.stringify(sheettoSave)
    }

    getSheetByName(sheetName: string){
        return this.sheets.find(sheet => sheet.name == sheetName)
    }
}