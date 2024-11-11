
import { TokenTypes, Errors, Token} from "./defination";
interface TempToken {
    value: string;
    start: number;
    end: number;
    type: keyof typeof TokenTypes;
}
const Regexes = {
    // 'Whitespace':/\s/g,
    // 'formulaName': /^(?:[A-Za-z.]{1,}[A-Za-z_0-9]+(?=[(]))/g,
    // 'CellRange':/\b([A-Z]+\d+):([A-Z]+\d+)\b/g,

    // 'DoubleQ':/\"/g,
    // 'QuotedString': /"([^"]*)"/g,
    // 'SingleQ':/\'\w*\'/g,
    'Cell': /\b[A-Z]+\d+\b/g, // Matches one or more uppercase letters followed by one or more digits, as whole words
    'Number': /\b(?![A-Z]+\d+)(?:[0-9]+(?:\.[0-9]+)?)\b/g, // Matches only digits, but not patterns like A1
    'formulaName':  /^(?:[A-Za-z.]{1,}[A-Za-z_0-9]+(?=[(]))/g, // Matches one or more letters (case insensitive)
    'DoubleQ':/"(\\["]|[^"])*"/g,
    'addOp' : /\+/g,
    'mulOp' : /\*/g,
    'divOp' : /\//g,
    'subOp' :/-/g,
    'Whitespace':/\s/g,
    'rangeOp':/:/g,

    'TRUE':/true/g,
    'FALSE':/false/g,
    'CLOSE_PAREN': /(?:\))/g,
    'OPEN_PAREN':/(?:\()/g,
    'Comma':/(?:,)/g,

    'SQUARE_OPEN':/(?:\[)/g,
    'SQUARE_CLOSE': /(?:\])/g
    
}





export class Scanner {
    line: string;
    slice: string;
    tokens: Array<Token>;

    constructor(line: string) {
        this.line = line;
        this.tokens = [];
        this.slice = line;
    }


    scanTokens() {
        // console.log("Scanner is ready");
        // console.log(this.line)
        let all_tokens: TempToken[] = []
        // for (let c = 0; c < this.line.length; c++) {
        //     const element = this.line[c];
        //     console.log(element);
        // }
        for (const [key, value] of Object.entries(Regexes)) {
            let matches = this.line.matchAll(value);
            for (const match of matches) {
                let temp = {
                    value:match[0],
                    start:match.index,
                    end:match.index + match[0].length,
                    type:key as keyof typeof TokenTypes
                }
                temp = <TempToken> temp
                all_tokens.push(temp)
            }
        }
        all_tokens.sort((a, b) => a.end - b.end);

        all_tokens = this.cleanTokens(all_tokens)
        // all tokens are sorted
        all_tokens.forEach(value =>{
            // console.log(value)
            this.tokens.push(new Token(value.value,TokenTypes[value.type as keyof typeof TokenTypes]))
        })


    }
    getTokens(){
        return this.tokens
    }

    cleanTokens(temptokens: TempToken[]) {
        let arr = temptokens;
        let splice_indexes = []
        for (let index = 0; index < arr.length; index++) {
            let item = arr[index];
            if (item.type === "DoubleQ") {
                for (let i = index; i > 0; i--) {
                    if (item.start == arr[i].end) {
                        let temp = { 
                            start: i+1,
                            length: arr.indexOf(item) - (i+1)
                        }
                        splice_indexes.push(temp)
                    }
                }
            }
        }

        // splice them 
        for (var i = splice_indexes.length -1; i >= 0; i--){
            arr.splice(splice_indexes[i].start,splice_indexes[i].length);
        }

        return arr
    }
    
    printTokens() {
        this.tokens.forEach(token => {
            console.log(`Type: ${TokenTypes[token.type]}, Value: ${token.value}`);
        });
    }
}
