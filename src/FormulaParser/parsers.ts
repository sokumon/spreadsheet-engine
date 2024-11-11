import { Token, Value } from "./defination";
import { Expr, Unary, Literal, Binary, Grouping , Call, EArray} from "./ast";
import { TokenTypes } from "./defination";
import { Cell } from "../cell";

export class Parser {
    tokens: Array<Token>;
    current: number;
    cells:Record<string,Cell>
    constructor(tokens: Array<Token>) {
        this.tokens = tokens;
        this.current = 0;
        this.cells = {}
    }

    private start(){
        return this.expression()
    }
    private expression(): Expr {
        return this.term();
    }

    private term(): Expr {
        let left: Expr = this.factor();

        while (this.match(TokenTypes.addOp, TokenTypes.subOp)) {
            const operator: Token = this.previous();
            const right: Expr = this.factor();
            left = new Binary(left, operator, right);
        }

        return left;
    }

    private factor(): Expr {
        let left: Expr = this.unary();

        while (this.match(TokenTypes.divOp, TokenTypes.mulOp)) {
            const operator: Token = this.previous();
            const right: Expr = this.unary();

            left = new Binary(left, operator, right);
        }

        return left;
    }

    private unary(): Expr {
        if (this.match(TokenTypes.subOp)) {
            const operator: Token = this.previous();
            const right: Expr = this.unary();
            return new Unary(operator, right);
        }

        return this.call();
    }

    private call(): Expr {
        let expr = this.primary();
    
        while (true) {
            if (this.match(TokenTypes.OPEN_PAREN)) {
                expr = this.finishCall(expr);
            } else {
                break; 
            }
        }
    
        return expr;
    }
    

    private finishCall(callee: Expr): Expr{
        let args: Array<Expr> = [];
        if(!this.check(TokenTypes.CLOSE_PAREN)){
            do{
                if(this.peek().type == TokenTypes.Whitespace){
                    this.advance()
                }
                args.push(this.expression())
            } while(this.match(TokenTypes.Comma))
        }
        let paren: Token = this.consume(TokenTypes.CLOSE_PAREN,"Except ) after the branch")
        return new Call(callee,paren,args)
    }
    private primary(): Expr {
        if (this.match(TokenTypes.OPEN_PAREN)) {
            const expr: Expr = this.expression();
            this.consume(TokenTypes.CLOSE_PAREN, "Expect ')' after expression");
            return new Grouping(expr);
        }

        if(this.match(TokenTypes.SQUARE_OPEN)){
            let elements: Array<Expr> = [];
            if(!this.check(TokenTypes.CLOSE_PAREN)){
                do{
                    if(this.peek().type == TokenTypes.Whitespace){
                        this.advance()
                    }
                    elements.push(this.expression())
                } while(this.match(TokenTypes.Comma))
            }
            let paren: Token = this.consume(TokenTypes.SQUARE_CLOSE,"Except ) after the branch")
            return new EArray(elements)
        }
    
        if (this.match(TokenTypes.FALSE)) return new Literal(false);
        if (this.match(TokenTypes.TRUE)) return new Literal(true);
        if (this.match(TokenTypes.Cell)) { 
            const start = this.previous().value; 
            if (this.match(TokenTypes.rangeOp)){

                if (!this.match(TokenTypes.Cell)) {
                    throw new Error("Expect second cell reference after ':'");
                }
                
                const end = this.previous().value;
                let cellRangeValues =  this.fetchCellRange(`${start}:${end}`)
                return new Literal(cellRangeValues); 
            }
            
            let value = this.fetchCellValue(start)
            return new Literal(value!)
        }

        if (this.match(TokenTypes.DoubleQ)) {
            return new Literal(this.previous().value.replaceAll('"',''));
        }
        if (this.match(TokenTypes.formulaName)) {
            return new Literal(this.previous().value.replaceAll('"','').toLowerCase());
        }

        if (this.match(TokenTypes.Text)) {
            return new Literal(this.previous().value.replaceAll('"',''));
        }
        if (this.match(TokenTypes.Number)) {
            return new Literal(parseFloat(this.previous().value));
        }
        throw new Error("Unexpected token");
    }

    private consume(type: TokenTypes, message: string): Token {
        if (this.check(type)) {
            return this.advance();
        }
        throw new Error(message);
    }

    private match(...types: TokenTypes[]): boolean {
        for (const type of types) {
            if (this.check(type)) {
                this.advance();
                return true;
            }
        }
        return false;
    }

    private check(type: TokenTypes): boolean {
        if (this.isAtEnd()) return false;
        return this.peek().type === type;
    }

    private advance(): Token {
        if (!this.isAtEnd()) this.current++;
        return this.previous();
    }

    private peek(): Token {
        return this.tokens[this.current];
    }

    private previous(): Token {
        return this.tokens[this.current - 1];
    }

    private isAtEnd(): boolean {
        return this.current === this.tokens.length;
    }

    public parse(): Expr {
        return this.start();
    }

    fetchCellValue(key: string){
        if(this.cells[key]){
            if(this.cells[key].type == TokenTypes.Number){
                return parseInt(this.cells[key].value?.toString()!)
            }
            return this.cells[key].value?.toString()
        }else{
            return " "
        }
    }

    fetchCellRange(range: string){
        let startCell = range.split(":")[0]
        let endCell = range.split(":")[1]

        let startCol = startCell.charAt(0)
        let endCol = endCell.charAt(0)
        let startRow = parseInt(startCell.charAt(1))
        let endRow = parseInt(endCell.charAt(1))

        let cells_choosen = []

        if(startCol == endCol){
            // deals with vertical case A1:A10
            for(let i=startRow; i <= endRow;i++){
                cells_choosen.push(`${startCol.toString()}${i.toString()}`)
            }
        }else if(startRow == endRow){
            // deals with horizontal case B2:G2
            for(let i=startCol.charCodeAt(0); i <= endCol.charCodeAt(0);i++){
                cells_choosen.push(`${String.fromCharCode(i)}${startRow.toString()}`)
            }
        }else{
            // deals with rectangular box case B2:E6
            // )
            for(let j=startRow; j <= endRow;j++){
                for(let i=startCol.charCodeAt(0); i <= endCol.charCodeAt(0);i++){
                    cells_choosen.push(`${String.fromCharCode(i)}${j.toString()}`)
                }
            }
        }
        
        let cell_values: Array<any> = []
        cells_choosen.forEach(cell => {
            cell_values.push(this.fetchCellValue(cell))
        })
        return cell_values
        
    }
}
