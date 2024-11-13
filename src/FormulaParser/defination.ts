export enum TokenTypes {
    Formula,
    Text,
    Number,
    Cell,

    addOp,
    mulOp,
    divOp,
    subOp,

    //Equality Operators
    EQUAL_EQUAL,BANG_EQUAL,

    //Unary Operators
    BANG,

    //Boolean 
    TRUE,FALSE,

    //Range Operator
    rangeOp,
    Whitespace,
    formulaName,
    CellRange,
    OPEN_PAREN,
    CLOSE_PAREN,
    SQUARE_OPEN,
    SQUARE_CLOSE,

    DoubleQ,
    Comma,
    //EOF
    EOF

}

export class Token {
  value: string;
  type: TokenTypes;

  constructor(value: string, type: TokenTypes) {
      this.value = value;
      this.type = type;
  }
}
export enum CellType{
    Formula,
    String,
    Number
}

export interface Cell{
    value: string,
    type: TokenTypes.Text | TokenTypes.Number | TokenTypes.Formula
}

export interface Sheet{
    cells: Record<string,Cell>
}

export enum Errors {
    CantParse = "Cant Parse",
    UnknownRangeName = "Unknown Range Name",
    WrongParameterType = "Type for the paramemeter doesnt match",
    CircularDependencyError = "Theres a circular dependency"
}

// export const sheet: Sheet = {
//     cells: {
//       'a1': {
//         value: '1',
//         type: TokenTypes.Number,
//       },
//       'a2': {
//         value: '2',
//         type: TokenTypes.Number,
//       },
//       'b1': {
//         value: '3',
//         type: TokenTypes.Number,
//       },
//       'b2': {
//         value: '=SUM(a1, a2)',
//         type: TokenTypes.Formula,
//       }
//     }
// }

export type Value = null | string | number | boolean | object | Array<Value>;