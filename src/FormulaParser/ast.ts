import { Token, TokenTypes } from "./defination";

export interface Expr {
    accept<R>(visitor: Visitor<R>): R;
}
  
export interface Visitor<R> {
    visitBinaryExpr(expr: Binary): R ;
    // visitAssignExpr(expr: Assign): R;
    visitCallExpr(expr: Call): R;
    // visitGetExpr(expr: Get): R;
    visitGroupingExpr(expr: Grouping): R;
    visitLiteralExpr(expr: Literal): R ;
    // visitSetExpr(expr: Set): R;
    // visitSuperExpr(expr: Super): R;
    // visitThisExpr(expr: This): R;
    visitUnaryExpr(expr: Unary): R ;
    // visitVariableExpr(expr: Variable): R;
    visitArrayExpr(expr: EArray): R;

}

export class Binary implements Expr {
    public left: Expr;
    public operator: Token;
    public right: Expr
    constructor(
       left: Expr,
      operator: Token,
      right: Expr
    ) {
      this.left = left;
      this.operator = operator;
      this.right = right
    }
  
    accept<R>(visitor: Visitor<R>): R{
      return visitor.visitBinaryExpr(this);
    }
  }
  
export class Grouping implements Expr{
    public expression:Expr;
    constructor(
        expression: Expr
    ){
        
        this.expression =  expression;
    }

    accept<R>(visitor: Visitor<R>): R{
        return visitor.visitGroupingExpr(this)
    }
}

export class Unary implements Expr{
    public operator:Token;
    public expression: Expr;
    constructor(
        operator:Token,
        expression: Expr
    ){
        
        this.operator = operator;
        this.expression = expression;
    }

    accept<R>(visitor: Visitor<R>): R {
        return visitor.visitUnaryExpr(this)
    }
}

export class Literal implements Expr{
    public value: Object
    constructor(value: Object){
        this.value = value
    }

    accept<R>(visitor: Visitor<R>): R{
        return visitor.visitLiteralExpr(this)
    }
}
export class EArray implements Expr{ 
    public elements: Array<Expr>
    constructor(value: Array<Expr>){
        this.elements = value
    }
    accept<R>(visitor: Visitor<R>): R{
        return visitor.visitArrayExpr(this)
    }
}
export class Call implements Expr{
    public callee: Expr;
    public paren: Token;
    public args: Array<Expr>;
    constructor(callee: Expr,paren: Token,args: Array<Expr>){
        this.args = args;
        this.paren = paren;
        this.callee = callee
    }

    accept<R>(visitor: Visitor<R>): R {
        return visitor.visitCallExpr(this)
    }
}