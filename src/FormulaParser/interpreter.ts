import { Binary, Call, EArray, Expr, Grouping, Literal, Unary, Visitor } from "./ast";
import { Token, TokenTypes } from "./defination";
import { formulas } from "./formula";

type Value = null | string | number | boolean | object | Array<Value>;

export class Interpreter implements Visitor<Value> {
    private evaluate(expr: Expr): Value {
        return expr.accept(this);
    }

    visitLiteralExpr(expr: Literal): Value {
        return expr.value;
    }

    visitUnaryExpr(expr: Unary): Value {
        const right = this.evaluate(expr.expression);
        switch (expr.operator.type) {
            case TokenTypes.subOp:
                return -Number(right);
        }
        return new Error("Unary Expression failed");
    }

    visitBinaryExpr(expr: Binary): Value {
        const left = this.evaluate(expr.left);
        const right = this.evaluate(expr.right);

        switch (expr.operator.type) {
            case TokenTypes.addOp:
                return Number(left) + Number(right);
            case TokenTypes.subOp:
                return Number(left) - Number(right);
            case TokenTypes.mulOp:
                return Number(left) * Number(right);
            case TokenTypes.divOp:
                if (Number(right) === 0) {
                    throw new Error("Division by zero");
                }
                return Number(left) / Number(right);
        }

        return new Error("Binary Expression Failed");
    }

    visitGroupingExpr(expr: Grouping): Value {
        return this.evaluate(expr.expression);
    }

    visitCallExpr(expr: Call): Value {
        let callee: Value =this.evaluate(expr.callee);
        let args: Array<Value> = [];
        expr.args.forEach(arg =>{
            args.push(this.evaluate(arg))
        })
        
        if (typeof callee == "string" && callee in formulas){
            return formulas[callee.toLowerCase() as keyof typeof formulas].compute(args)
        }
        return new Error("callee is of the wrong type")
  
    }

    visitArrayExpr(expr: EArray): Value {
        let array = new Array()
        expr.elements.forEach(arg =>{
            array.push(this.evaluate(arg))
        })
        console.log(array)
        return array
    }

    interpret(expression: Expr): Value {
        const value: Value = this.evaluate(expression);
        return value
    }
} 
