import * as formulajs from '@formulajs/formulajs';

export const formulas = {

    date: {
        desc: "Converts a provided year, month, and day into a date. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("DATE requires exactly 3 arguments.");
            }
            return formulajs.DATE.apply(null, values as [Value, Value, Value]);
        },
        completion: "DATE("
    },

    datevalue: {
        desc: "Converts a provided date string in a known format to a date value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DATEVALUE requires exactly 1 arguments.");
            }
            return formulajs.DATEVALUE.apply(null, values as [Value]);
        },
        completion: "DATEVALUE("
    },

    day: {
        desc: "Returns the day of the month that a specific date falls on, in numeric format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DAY requires exactly 1 arguments.");
            }
            return formulajs.DAY.apply(null, values as [Value]);
        },
        completion: "DAY("
    },

    days: {
        desc: "Returns the number of days between two dates. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("DAYS requires exactly 2 arguments.");
            }
            return formulajs.DAYS.apply(null, values as [Value, Value]);
        },
        completion: "DAYS("
    },

    days360: {
        desc: "Returns the difference between two days based on the 360 day year used in some financial interest calculations. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("DAYS360 requires exactly 2 arguments.");
            }
            return formulajs.DAYS360.apply(null, values as [Value, Value]);
        },
        completion: "DAYS360("
    },

    edate: {
        desc: "Returns a date a specified number of months before or after another date. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("EDATE requires exactly 2 arguments.");
            }
            return formulajs.EDATE.apply(null, values as [Value, Value]);
        },
        completion: "EDATE("
    },

    eomonth: {
        desc: "Returns a date representing the last day of a month which falls a specified number of months before or after another date. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("EOMONTH requires exactly 2 arguments.");
            }
            return formulajs.EOMONTH.apply(null, values as [Value, Value]);
        },
        completion: "EOMONTH("
    },

    hour: {
        desc: "Returns the hour component of a specific time, in numeric format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("HOUR requires exactly 1 arguments.");
            }
            return formulajs.HOUR.apply(null, values as [Value]);
        },
        completion: "HOUR("
    },

    minute: {
        desc: "Returns the minute component of a specific time, in numeric format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("MINUTE requires exactly 1 arguments.");
            }
            return formulajs.MINUTE.apply(null, values as [Value]);
        },
        completion: "MINUTE("
    },

    isoweeknum: {
        desc: "Returns the number of the ISO week of the year where the provided date falls. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ISOWEEKNUM requires exactly 1 arguments.");
            }
            return formulajs.ISOWEEKNUM.apply(null, values as [Value]);
        },
        completion: "ISOWEEKNUM("
    },

    month: {
        desc: "Returns the month of the year a specific date falls in, in numeric format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("MONTH requires exactly 1 arguments.");
            }
            return formulajs.MONTH.apply(null, values as [Value]);
        },
        completion: "MONTH("
    },

    networkdays: {
        desc: "Returns the number of net working days between two provided days. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("NETWORKDAYS requires exactly 3 arguments.");
            }
            return formulajs.NETWORKDAYS.apply(null, values as [Value, Value, Value]);
        },
        completion: "NETWORKDAYS("
    },

    now: {
        desc: "Returns the current date and time as a date value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 0) {
                throw new Error("NOW requires exactly 0 arguments.");
            }
            return formulajs.NOW.apply(null, values as []);
        },
        completion: "NOW("
    },

    second: {
        desc: "Returns the second component of a specific time, in numeric format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SECOND requires exactly 1 arguments.");
            }
            return formulajs.SECOND.apply(null, values as [Value]);
        },
        completion: "SECOND("
    },

    time: {
        desc: "Converts a provided hour, minute, and second into a time. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("TIME requires exactly 3 arguments.");
            }
            return formulajs.TIME.apply(null, values as [Value, Value, Value]);
        },
        completion: "TIME("
    },

    timevalue: {
        desc: "Returns the fraction of a 24-hour day the time represents. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("TIMEVALUE requires exactly 1 arguments.");
            }
            return formulajs.TIMEVALUE.apply(null, values as [Value]);
        },
        completion: "TIMEVALUE("
    },

    today: {
        desc: "Returns the current date as a date value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 0) {
                throw new Error("TODAY requires exactly 0 arguments.");
            }
            return formulajs.TODAY.apply(null, values as []);
        },
        completion: "TODAY("
    },

    weekday: {
        desc: "Returns a number representing the day of the week of the date provided. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("WEEKDAY requires exactly 2 arguments.");
            }
            return formulajs.WEEKDAY.apply(null, values as [Value, Value]);
        },
        completion: "WEEKDAY("
    },

    year: {
        desc: "Returns the year specified by a given date. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("YEAR requires exactly 1 arguments.");
            }
            return formulajs.YEAR.apply(null, values as [Value]);
        },
        completion: "YEAR("
    },

    weeknum: {
        desc: "Returns a number representing the week of the year where the provided date falls. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("WEEKNUM requires exactly 2 arguments.");
            }
            return formulajs.WEEKNUM.apply(null, values as [Value, Value]);
        },
        completion: "WEEKNUM("
    },

    workday: {
        desc: "Calculates the end date after a specified number of working days. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("WORKDAY requires exactly 4 arguments.");
            }
            return formulajs.WORKDAY.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "WORKDAY("
    },

    yearfrac: {
        desc: "Returns the number of years, including fractional years, between two dates using a specified day count convention. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("YEARFRAC requires exactly 3 arguments.");
            }
            return formulajs.YEARFRAC.apply(null, values as [Value, Value, Value]);
        },
        completion: "YEARFRAC("
    },

    accrint: {
        desc: "Calculates the accrued interest of a security that has periodic payments. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 7) {
                throw new Error("ACCRINT requires exactly 7 arguments.");
            }
            return formulajs.ACCRINT.apply(null, values as [Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "ACCRINT("
    },

    cumipmt: {
        desc: "Calculates the cumulative interest over a range of payment periods for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("CUMIPMT requires exactly 6 arguments.");
            }
            return formulajs.CUMIPMT.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "CUMIPMT("
    },

    cumprinc: {
        desc: "Calculates the cumulative principal paid over a range of payment periods for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("CUMPRINC requires exactly 6 arguments.");
            }
            return formulajs.CUMPRINC.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "CUMPRINC("
    },

    db: {
        desc: "Calculates the depreciation of an asset for a specified period using the arithmetic declining balance method. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("DB requires exactly 5 arguments.");
            }
            return formulajs.DB.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "DB("
    },

    ddb: {
        desc: "Calculates the depreciation of an asset for a specified period using the double-declining balance method. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("DDB requires exactly 5 arguments.");
            }
            return formulajs.DDB.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "DDB("
    },

    dollarde: {
        desc: "Converts a price quotation given as a decimal fraction into a decimal value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("DOLLARDE requires exactly 2 arguments.");
            }
            return formulajs.DOLLARDE.apply(null, values as [Value, Value]);
        },
        completion: "DOLLARDE("
    },

    dollarfr: {
        desc: "Converts a price quotation given as a decimal value into a decimal fraction. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("DOLLARFR requires exactly 2 arguments.");
            }
            return formulajs.DOLLARFR.apply(null, values as [Value, Value]);
        },
        completion: "DOLLARFR("
    },

    effect: {
        desc: "Calculates the annual effective interest rate given the nominal rate and number of compounding periods per year. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("EFFECT requires exactly 2 arguments.");
            }
            return formulajs.EFFECT.apply(null, values as [Value, Value]);
        },
        completion: "EFFECT("
    },

    fv: {
        desc: "Calculates the future value of an annuity investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("FV requires exactly 5 arguments.");
            }
            return formulajs.FV.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "FV("
    },

    fvschedule: {
        desc: "Calculates the future value of some principal based on a specified series of potentially varying interest rates. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("FVSCHEDULE requires exactly 4 arguments.");
            }
            return formulajs.FVSCHEDULE.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "FVSCHEDULE("
    },

    ipmt: {
        desc: "Calculates the payment on interest for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("IPMT requires exactly 6 arguments.");
            }
            return formulajs.IPMT.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "IPMT("
    },

    irr: {
        desc: "Calculates the internal rate of return on an investment based on a series of periodic cash flows. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("IRR requires exactly 2 arguments.");
            }
            return formulajs.IRR.apply(null, values as [Value, Value]);
        },
        completion: "IRR("
    },

    ispmt: {
        desc: "The ISPMT function calculates the interest paid during a particular period of an investment. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("ISPMT requires exactly 4 arguments.");
            }
            return formulajs.ISPMT.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "ISPMT("
    },

    mirr: {
        desc: "Calculates the modified internal rate of return on an investment based on a series of periodic cash flows and the difference between the interest rate paid on financing versus the return received on reinvested income. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("MIRR requires exactly 3 arguments.");
            }
            return formulajs.MIRR.apply(null, values as [Value, Value, Value]);
        },
        completion: "MIRR("
    },

    nominal: {
        desc: "Calculates the annual nominal interest rate given the effective rate and number of compounding periods per year. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("NOMINAL requires exactly 2 arguments.");
            }
            return formulajs.NOMINAL.apply(null, values as [Value, Value]);
        },
        completion: "NOMINAL("
    },

    nper: {
        desc: "Calculates the number of payment periods for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("NPER requires exactly 5 arguments.");
            }
            return formulajs.NPER.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "NPER("
    },

    npv: {
        desc: "Calculates the net present value of an investment based on a series of periodic cash flows and a discount rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("NPV requires exactly 5 arguments.");
            }
            return formulajs.NPV.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "NPV("
    },

    pduration: {
        desc: "Returns the number of periods for an investment to reach a specific value at a given rate. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("PDURATION requires exactly 3 arguments.");
            }
            return formulajs.PDURATION.apply(null, values as [Value, Value, Value]);
        },
        completion: "PDURATION("
    },

    pmt: {
        desc: "Calculates the periodic payment for an annuity investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("PMT requires exactly 5 arguments.");
            }
            return formulajs.PMT.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "PMT("
    },

    ppmt: {
        desc: "Calculates the payment on the principal of an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("PPMT requires exactly 6 arguments.");
            }
            return formulajs.PPMT.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "PPMT("
    },

    pv: {
        desc: "Calculates the present value of an annuity investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("PV requires exactly 5 arguments.");
            }
            return formulajs.PV.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "PV("
    },

    rate: {
        desc: "Calculates the interest rate of an annuity investment based on constant-amount periodic payments and the assumption of a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("RATE requires exactly 6 arguments.");
            }
            return formulajs.RATE.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "RATE("
    },

    bin2dec: {
        desc: "Converts a signed binary number to decimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("BIN2DEC requires exactly 1 arguments.");
            }
            return formulajs.BIN2DEC.apply(null, values as [Value]);
        },
        completion: "BIN2DEC("
    },

    bin2hex: {
        desc: "Converts a signed binary number to signed hexadecimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("BIN2HEX requires exactly 1 arguments.");
            }
            return formulajs.BIN2HEX.apply(null, values as [Value]);
        },
        completion: "BIN2HEX("
    },

    bin2oct: {
        desc: "Converts a signed binary number to signed octal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("BIN2OCT requires exactly 1 arguments.");
            }
            return formulajs.BIN2OCT.apply(null, values as [Value]);
        },
        completion: "BIN2OCT("
    },

    bitand: {
        desc: "Bitwise boolean AND of two numbers. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("BITAND requires exactly 2 arguments.");
            }
            return formulajs.BITAND.apply(null, values as [Value, Value]);
        },
        completion: "BITAND("
    },

    bitlshift: {
        desc: "Shifts the bits of the input a certain number of places to the left. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("BITLSHIFT requires exactly 2 arguments.");
            }
            return formulajs.BITLSHIFT.apply(null, values as [Value, Value]);
        },
        completion: "BITLSHIFT("
    },

    bitor: {
        desc: "Bitwise boolean OR of 2 numbers. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("BITOR requires exactly 2 arguments.");
            }
            return formulajs.BITOR.apply(null, values as [Value, Value]);
        },
        completion: "BITOR("
    },

    bitrshift: {
        desc: "Shifts the bits of the input a certain number of places to the right. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("BITRSHIFT requires exactly 2 arguments.");
            }
            return formulajs.BITRSHIFT.apply(null, values as [Value, Value]);
        },
        completion: "BITRSHIFT("
    },

    bitxor: {
        desc: "Bitwise XOR (exclusive OR) of 2 numbers. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("BITXOR requires exactly 2 arguments.");
            }
            return formulajs.BITXOR.apply(null, values as [Value, Value]);
        },
        completion: "BITXOR("
    },

    complex: {
        desc: "Creates a complex number given real and imaginary coefficients. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("COMPLEX requires exactly 2 arguments.");
            }
            return formulajs.COMPLEX.apply(null, values as [Value, Value]);
        },
        completion: "COMPLEX("
    },

    convert: {
        desc: "Converts a numeric value to a different unit of measure. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("CONVERT requires exactly 3 arguments.");
            }
            return formulajs.CONVERT.apply(null, values as [Value, Value, Value]);
        },
        completion: "CONVERT("
    },

    dec2bin: {
        desc: "Converts a decimal number to signed binary format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DEC2BIN requires exactly 1 arguments.");
            }
            return formulajs.DEC2BIN.apply(null, values as [Value]);
        },
        completion: "DEC2BIN("
    },

    dec2hex: {
        desc: "Converts a decimal number to signed hexadecimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DEC2HEX requires exactly 1 arguments.");
            }
            return formulajs.DEC2HEX.apply(null, values as [Value]);
        },
        completion: "DEC2HEX("
    },

    dec2oct: {
        desc: "Converts a decimal number to signed octal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DEC2OCT requires exactly 1 arguments.");
            }
            return formulajs.DEC2OCT.apply(null, values as [Value]);
        },
        completion: "DEC2OCT("
    },

    delta: {
        desc: "Compare two numeric values, returning 1 if they\'re equal. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("DELTA requires exactly 2 arguments.");
            }
            return formulajs.DELTA.apply(null, values as [Value, Value]);
        },
        completion: "DELTA("
    },

    erf: {
        desc: "The ERF function returns the integral of the Gauss error function over an interval of values. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ERF requires exactly 1 arguments.");
            }
            return formulajs.ERF.apply(null, values as [Value]);
        },
        completion: "ERF("
    },

    erfc: {
        desc: "Returns the complementary Gauss error function of a value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ERFC requires exactly 1 arguments.");
            }
            return formulajs.ERFC.apply(null, values as [Value]);
        },
        completion: "ERFC("
    },

    gestep: {
        desc: "Returns 1 if the rate is strictly greater than or equal to the provided step value or 0 otherwise. If no step value is provided then the default value of 0 will be used. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("GESTEP requires exactly 2 arguments.");
            }
            return formulajs.GESTEP.apply(null, values as [Value, Value]);
        },
        completion: "GESTEP("
    },

    hex2bin: {
        desc: "Converts a signed hexadecimal number to signed binary format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("HEX2BIN requires exactly 1 arguments.");
            }
            return formulajs.HEX2BIN.apply(null, values as [Value]);
        },
        completion: "HEX2BIN("
    },

    hex2dec: {
        desc: "Converts a signed hexadecimal number to decimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("HEX2DEC requires exactly 1 arguments.");
            }
            return formulajs.HEX2DEC.apply(null, values as [Value]);
        },
        completion: "HEX2DEC("
    },

    hex2oct: {
        desc: "Converts a signed hexadecimal number to signed octal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("HEX2OCT requires exactly 1 arguments.");
            }
            return formulajs.HEX2OCT.apply(null, values as [Value]);
        },
        completion: "HEX2OCT("
    },

    imabs: {
        desc: "Returns absolute value of a complex number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMABS requires exactly 1 arguments.");
            }
            return formulajs.IMABS.apply(null, values as [Value]);
        },
        completion: "IMABS("
    },

    imaginary: {
        desc: "Returns the imaginary coefficient of a complex number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMAGINARY requires exactly 1 arguments.");
            }
            return formulajs.IMAGINARY.apply(null, values as [Value]);
        },
        completion: "IMAGINARY("
    },

    imargument: {
        desc: "The IMARGUMENT function returns the angle (also known as the argument or \theta) of the given complex number in radians. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMARGUMENT requires exactly 1 arguments.");
            }
            return formulajs.IMARGUMENT.apply(null, values as [Value]);
        },
        completion: "IMARGUMENT("
    },

    imconjugate: {
        desc: "Returns the complex conjugate of a number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMCONJUGATE requires exactly 1 arguments.");
            }
            return formulajs.IMCONJUGATE.apply(null, values as [Value]);
        },
        completion: "IMCONJUGATE("
    },

    imcos: {
        desc: "The IMCOS function returns the cosine of the given complex number. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMCOS requires exactly 1 arguments.");
            }
            return formulajs.IMCOS.apply(null, values as [Value]);
        },
        completion: "IMCOS("
    },

    imcosh: {
        desc: "Returns the hyperbolic cosine of the given complex number. For example, a given complex number \"x+yi\" returns \"cosh(x+yi).\" .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMCOSH requires exactly 1 arguments.");
            }
            return formulajs.IMCOSH.apply(null, values as [Value]);
        },
        completion: "IMCOSH("
    },

    imcot: {
        desc: "Returns the cotangent of the given complex number. For example, a given complex number \"x+yi\" returns \"cot(x+yi).\" .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMCOT requires exactly 1 arguments.");
            }
            return formulajs.IMCOT.apply(null, values as [Value]);
        },
        completion: "IMCOT("
    },

    imcsc: {
        desc: "Returns the cosecant of the given complex number. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMCSC requires exactly 1 arguments.");
            }
            return formulajs.IMCSC.apply(null, values as [Value]);
        },
        completion: "IMCSC("
    },

    imcsch: {
        desc: "Returns the hyperbolic cosecant of the given complex number. For example, a given complex number \"x+yi\" returns \"csch(x+yi).\" .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMCSCH requires exactly 1 arguments.");
            }
            return formulajs.IMCSCH.apply(null, values as [Value]);
        },
        completion: "IMCSCH("
    },

    imdiv: {
        desc: "Returns one complex number divided by another. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("IMDIV requires exactly 2 arguments.");
            }
            return formulajs.IMDIV.apply(null, values as [Value, Value]);
        },
        completion: "IMDIV("
    },

    imexp: {
        desc: "Returns Euler\'s number, e (~2.718) raised to a complex power. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMEXP requires exactly 1 arguments.");
            }
            return formulajs.IMEXP.apply(null, values as [Value]);
        },
        completion: "IMEXP("
    },

    imln: {
        desc: "Returns the logarithm of a complex number, base e (Euler\'s number). ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMLN requires exactly 1 arguments.");
            }
            return formulajs.IMLN.apply(null, values as [Value]);
        },
        completion: "IMLN("
    },

    imlog10: {
        desc: "Returns the logarithm of a complex number with base 10. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMLOG10 requires exactly 1 arguments.");
            }
            return formulajs.IMLOG10.apply(null, values as [Value]);
        },
        completion: "IMLOG10("
    },

    imlog2: {
        desc: "Returns the logarithm of a complex number with base 2. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMLOG2 requires exactly 1 arguments.");
            }
            return formulajs.IMLOG2.apply(null, values as [Value]);
        },
        completion: "IMLOG2("
    },

    impower: {
        desc: "Returns a complex number raised to a power. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("IMPOWER requires exactly 2 arguments.");
            }
            return formulajs.IMPOWER.apply(null, values as [Value, Value]);
        },
        completion: "IMPOWER("
    },

    improduct: {
        desc: "Returns the result of multiplying a series of complex numbers together. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("IMPRODUCT requires exactly 3 arguments.");
            }
            return formulajs.IMPRODUCT.apply(null, values as [Value, Value, Value]);
        },
        completion: "IMPRODUCT("
    },

    imreal: {
        desc: "Returns the real coefficient of a complex number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMREAL requires exactly 1 arguments.");
            }
            return formulajs.IMREAL.apply(null, values as [Value]);
        },
        completion: "IMREAL("
    },

    imsec: {
        desc: "Returns the secant of the given complex number. For example, a given complex number \"x+yi\" returns \"sec(x+yi).\" .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMSEC requires exactly 1 arguments.");
            }
            return formulajs.IMSEC.apply(null, values as [Value]);
        },
        completion: "IMSEC("
    },

    imsech: {
        desc: "Returns the hyperbolic secant of the given complex number. For example, a given complex number \"x+yi\" returns \"sech(x+yi).\" .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMSECH requires exactly 1 arguments.");
            }
            return formulajs.IMSECH.apply(null, values as [Value]);
        },
        completion: "IMSECH("
    },

    imsin: {
        desc: "Returns the sine of the given complex number. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMSIN requires exactly 1 arguments.");
            }
            return formulajs.IMSIN.apply(null, values as [Value]);
        },
        completion: "IMSIN("
    },

    imsinh: {
        desc: "Returns the hyperbolic sine of the given complex number. For example, a given complex number \"x+yi\" returns \"sinh(x+yi).\" .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMSINH requires exactly 1 arguments.");
            }
            return formulajs.IMSINH.apply(null, values as [Value]);
        },
        completion: "IMSINH("
    },

    imsqrt: {
        desc: "Computes the square root of a complex number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMSQRT requires exactly 1 arguments.");
            }
            return formulajs.IMSQRT.apply(null, values as [Value]);
        },
        completion: "IMSQRT("
    },

    imsub: {
        desc: "Returns the difference between two complex numbers. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("IMSUB requires exactly 2 arguments.");
            }
            return formulajs.IMSUB.apply(null, values as [Value, Value]);
        },
        completion: "IMSUB("
    },

    imsum: {
        desc: "Returns the sum of a series of complex numbers. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("IMSUM requires exactly 3 arguments.");
            }
            return formulajs.IMSUM.apply(null, values as [Value, Value, Value]);
        },
        completion: "IMSUM("
    },

    imtan: {
        desc: "Returns the tangent of the given complex number. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMTAN requires exactly 1 arguments.");
            }
            return formulajs.IMTAN.apply(null, values as [Value]);
        },
        completion: "IMTAN("
    },

    oct2bin: {
        desc: "Converts a signed octal number to signed binary format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("OCT2BIN requires exactly 1 arguments.");
            }
            return formulajs.OCT2BIN.apply(null, values as [Value]);
        },
        completion: "OCT2BIN("
    },

    oct2dec: {
        desc: "Converts a signed octal number to decimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("OCT2DEC requires exactly 1 arguments.");
            }
            return formulajs.OCT2DEC.apply(null, values as [Value]);
        },
        completion: "OCT2DEC("
    },

    oct2hex: {
        desc: "Converts a signed octal number to signed hexadecimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("OCT2HEX requires exactly 1 arguments.");
            }
            return formulajs.OCT2HEX.apply(null, values as [Value]);
        },
        completion: "OCT2HEX("
    },

    and: {
        desc: "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("AND requires exactly 3 arguments.");
            }
            return formulajs.AND.apply(null, values as [Value, Value, Value]);
        },
        completion: "AND("
    },

    if: {
        desc: "Returns one value if a logical expression is `TRUE` and another if it is `FALSE`. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("IF requires exactly 3 arguments.");
            }
            return formulajs.IF.apply(null, values as [Value, Value, Value]);
        },
        completion: "IF("
    },

    ifs: {
        desc: "Evaluates multiple conditions and returns a value that corresponds to the first true condition. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("IFS requires exactly 4 arguments.");
            }
            return formulajs.IFS.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "IFS("
    },

    iferror: {
        desc: "Returns the first argument if it is not an error value, otherwise returns the second argument if present, or a blank if the second argument is absent. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("IFERROR requires exactly 2 arguments.");
            }
            return formulajs.IFERROR.apply(null, values as [Value, Value]);
        },
        completion: "IFERROR("
    },

    ifna: {
        desc: "Evaluates a value. If the value is an #N/A error, returns the specified value. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("IFNA requires exactly 2 arguments.");
            }
            return formulajs.IFNA.apply(null, values as [Value, Value]);
        },
        completion: "IFNA("
    },

    not: {
        desc: "Returns the opposite of a logical value - `NOT(TRUE)` returns `FALSE`; `NOT(FALSE)` returns `TRUE`. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("NOT requires exactly 1 arguments.");
            }
            return formulajs.NOT.apply(null, values as [Value]);
        },
        completion: "NOT("
    },

    or: {
        desc: "Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("OR requires exactly 3 arguments.");
            }
            return formulajs.OR.apply(null, values as [Value, Value, Value]);
        },
        completion: "OR("
    },

    switch: {
        desc: "Tests an expression against a list of cases and returns the corresponding value of the first matching case, with an optional default value if nothing else is met. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("SWITCH requires exactly 5 arguments.");
            }
            return formulajs.SWITCH.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "SWITCH("
    },

    xor: {
        desc: "The XOR function performs an exclusive or of 2 numbers that returns a 1 if the numbers are different, and a 0 otherwise. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("XOR requires exactly 3 arguments.");
            }
            return formulajs.XOR.apply(null, values as [Value, Value, Value]);
        },
        completion: "XOR("
    },

    abs: {
        desc: "Returns the absolute value of a number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ABS requires exactly 1 arguments.");
            }
            return formulajs.ABS.apply(null, values as [Value]);
        },
        completion: "ABS("
    },

    acos: {
        desc: "Returns the inverse cosine of a value, in radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ACOS requires exactly 1 arguments.");
            }
            return formulajs.ACOS.apply(null, values as [Value]);
        },
        completion: "ACOS("
    },

    acosh: {
        desc: "Returns the inverse hyperbolic cosine of a number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ACOSH requires exactly 1 arguments.");
            }
            return formulajs.ACOSH.apply(null, values as [Value]);
        },
        completion: "ACOSH("
    },

    acot: {
        desc: "Returns the inverse cotangent of a value, in radians. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ACOT requires exactly 1 arguments.");
            }
            return formulajs.ACOT.apply(null, values as [Value]);
        },
        completion: "ACOT("
    },

    acoth: {
        desc: "Returns the inverse hyperbolic cotangent of a value, in radians. Must not be between -1 and 1, inclusive. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ACOTH requires exactly 1 arguments.");
            }
            return formulajs.ACOTH.apply(null, values as [Value]);
        },
        completion: "ACOTH("
    },

    arabic: {
        desc: "Computes the value of a Roman numeral. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ARABIC requires exactly 1 arguments.");
            }
            return formulajs.ARABIC.apply(null, values as [Value]);
        },
        completion: "ARABIC("
    },

    asin: {
        desc: "Returns the inverse sine of a value, in radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ASIN requires exactly 1 arguments.");
            }
            return formulajs.ASIN.apply(null, values as [Value]);
        },
        completion: "ASIN("
    },

    asinh: {
        desc: "Returns the inverse hyperbolic sine of a number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ASINH requires exactly 1 arguments.");
            }
            return formulajs.ASINH.apply(null, values as [Value]);
        },
        completion: "ASINH("
    },

    atan: {
        desc: "Returns the inverse tangent of a value, in radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ATAN requires exactly 1 arguments.");
            }
            return formulajs.ATAN.apply(null, values as [Value]);
        },
        completion: "ATAN("
    },

    atan2: {
        desc: "Returns the angle between the x-axis and a line segment from the origin (0,0) to specified coordinate pair (`x`,`y`), in radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("ATAN2 requires exactly 2 arguments.");
            }
            return formulajs.ATAN2.apply(null, values as [Value, Value]);
        },
        completion: "ATAN2("
    },

    atanh: {
        desc: "Returns the inverse hyperbolic tangent of a number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ATANH requires exactly 1 arguments.");
            }
            return formulajs.ATANH.apply(null, values as [Value]);
        },
        completion: "ATANH("
    },

    base: {
        desc: "Converts a number into a text representation in another base, for example, base 2 for binary. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("BASE requires exactly 3 arguments.");
            }
            return formulajs.BASE.apply(null, values as [Value, Value, Value]);
        },
        completion: "BASE("
    },

    ceiling: {
        desc: "Rounds a number up to the nearest integer multiple of specified significance. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("CEILING requires exactly 3 arguments.");
            }
            return formulajs.CEILING.apply(null, values as [Value, Value, Value]);
        },
        completion: "CEILING("
    },

    combin: {
        desc: "Returns the number of ways to choose some number of objects from a pool of a given size of objects. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("COMBIN requires exactly 2 arguments.");
            }
            return formulajs.COMBIN.apply(null, values as [Value, Value]);
        },
        completion: "COMBIN("
    },

    combina: {
        desc: "Returns the number of ways to choose some number of objects from a pool of a given size of objects, including ways that choose the same object multiple times. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("COMBINA requires exactly 2 arguments.");
            }
            return formulajs.COMBINA.apply(null, values as [Value, Value]);
        },
        completion: "COMBINA("
    },

    cos: {
        desc: "Returns the cosine of an angle provided in radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("COS requires exactly 1 arguments.");
            }
            return formulajs.COS.apply(null, values as [Value]);
        },
        completion: "COS("
    },

    cosh: {
        desc: "Returns the hyperbolic cosine of any real number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("COSH requires exactly 1 arguments.");
            }
            return formulajs.COSH.apply(null, values as [Value]);
        },
        completion: "COSH("
    },

    cot: {
        desc: "Cotangent of an angle provided in radians. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("COT requires exactly 1 arguments.");
            }
            return formulajs.COT.apply(null, values as [Value]);
        },
        completion: "COT("
    },

    coth: {
        desc: "Returns the hyperbolic cotangent of any real number. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("COTH requires exactly 1 arguments.");
            }
            return formulajs.COTH.apply(null, values as [Value]);
        },
        completion: "COTH("
    },

    csc: {
        desc: "Returns the cosecant of an angle provided in radians. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("CSC requires exactly 1 arguments.");
            }
            return formulajs.CSC.apply(null, values as [Value]);
        },
        completion: "CSC("
    },

    csch: {
        desc: "The CSCH function returns the hyperbolic cosecant of any real number. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("CSCH requires exactly 1 arguments.");
            }
            return formulajs.CSCH.apply(null, values as [Value]);
        },
        completion: "CSCH("
    },

    decimal: {
        desc: "The DECIMAL function converts the text representation of a number in another base, to base 10 (decimal). .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("DECIMAL requires exactly 2 arguments.");
            }
            return formulajs.DECIMAL.apply(null, values as [Value, Value]);
        },
        completion: "DECIMAL("
    },

    erf: {
        desc: "The ERF function returns the integral of the Gauss error function over an interval of values. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ERF requires exactly 1 arguments.");
            }
            return formulajs.ERF.apply(null, values as [Value]);
        },
        completion: "ERF("
    },

    erfc: {
        desc: "Returns the complementary Gauss error function of a value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ERFC requires exactly 1 arguments.");
            }
            return formulajs.ERFC.apply(null, values as [Value]);
        },
        completion: "ERFC("
    },

    even: {
        desc: "Rounds a number up to the nearest even integer. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("EVEN requires exactly 1 arguments.");
            }
            return formulajs.EVEN.apply(null, values as [Value]);
        },
        completion: "EVEN("
    },

    exp: {
        desc: "Returns Euler\'s number, e (~2.718) raised to a power. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("EXP requires exactly 1 arguments.");
            }
            return formulajs.EXP.apply(null, values as [Value]);
        },
        completion: "EXP("
    },

    fact: {
        desc: "Returns the factorial of a number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("FACT requires exactly 1 arguments.");
            }
            return formulajs.FACT.apply(null, values as [Value]);
        },
        completion: "FACT("
    },

    factdouble: {
        desc: "Returns the \"double factorial\" of a number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("FACTDOUBLE requires exactly 1 arguments.");
            }
            return formulajs.FACTDOUBLE.apply(null, values as [Value]);
        },
        completion: "FACTDOUBLE("
    },

    floor: {
        desc: "Rounds a number down to the nearest integer multiple of specified significance. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("FLOOR requires exactly 1 arguments.");
            }
            return formulajs.FLOOR.apply(null, values as [Value]);
        },
        completion: "FLOOR("
    },

    gcd: {
        desc: "Returns the greatest common divisor of one or more integers. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("GCD requires exactly 3 arguments.");
            }
            return formulajs.GCD.apply(null, values as [Value, Value, Value]);
        },
        completion: "GCD("
    },

    int: {
        desc: "Rounds a number down to the nearest integer that is less than or equal to it. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("INT requires exactly 1 arguments.");
            }
            return formulajs.INT.apply(null, values as [Value]);
        },
        completion: "INT("
    },

    iseven: {
        desc: "Checks whether the provided value is even. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ISEVEN requires exactly 1 arguments.");
            }
            return formulajs.ISEVEN.apply(null, values as [Value]);
        },
        completion: "ISEVEN("
    },

    isodd: {
        desc: "Checks whether the provided value is odd. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ISODD requires exactly 1 arguments.");
            }
            return formulajs.ISODD.apply(null, values as [Value]);
        },
        completion: "ISODD("
    },

    lcm: {
        desc: "Returns the least common multiple of one or more integers. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("LCM requires exactly 3 arguments.");
            }
            return formulajs.LCM.apply(null, values as [Value, Value, Value]);
        },
        completion: "LCM("
    },

    ln: {
        desc: "Returns the the logarithm of a number, base e (Euler\'s number). ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("LN requires exactly 1 arguments.");
            }
            return formulajs.LN.apply(null, values as [Value]);
        },
        completion: "LN("
    },

    log: {
        desc: "Returns the the logarithm of a number given a base. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("LOG requires exactly 2 arguments.");
            }
            return formulajs.LOG.apply(null, values as [Value, Value]);
        },
        completion: "LOG("
    },

    log10: {
        desc: "Returns the the logarithm of a number, base 10. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("LOG10 requires exactly 1 arguments.");
            }
            return formulajs.LOG10.apply(null, values as [Value]);
        },
        completion: "LOG10("
    },

    mod: {
        desc: "Returns the result of the modulo operator, the remainder after a division operation. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("MOD requires exactly 2 arguments.");
            }
            return formulajs.MOD.apply(null, values as [Value, Value]);
        },
        completion: "MOD("
    },

    mround: {
        desc: "Rounds one number to the nearest integer multiple of another. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("MROUND requires exactly 2 arguments.");
            }
            return formulajs.MROUND.apply(null, values as [Value, Value]);
        },
        completion: "MROUND("
    },

    multinomial: {
        desc: "Returns the factorial of the sum of values divided by the product of the values\' factorials. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("MULTINOMIAL requires exactly 3 arguments.");
            }
            return formulajs.MULTINOMIAL.apply(null, values as [Value, Value, Value]);
        },
        completion: "MULTINOMIAL("
    },

    odd: {
        desc: "Rounds a number up to the nearest odd integer. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ODD requires exactly 1 arguments.");
            }
            return formulajs.ODD.apply(null, values as [Value]);
        },
        completion: "ODD("
    },

    power: {
        desc: "Returns a number raised to a power. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("POWER requires exactly 2 arguments.");
            }
            return formulajs.POWER.apply(null, values as [Value, Value]);
        },
        completion: "POWER("
    },

    product: {
        desc: "Returns the result of multiplying a series of numbers together. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("PRODUCT requires exactly 3 arguments.");
            }
            return formulajs.PRODUCT.apply(null, values as [Value, Value, Value]);
        },
        completion: "PRODUCT("
    },

    quotient: {
        desc: "Returns one number divided by another. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("QUOTIENT requires exactly 2 arguments.");
            }
            return formulajs.QUOTIENT.apply(null, values as [Value, Value]);
        },
        completion: "QUOTIENT("
    },

    radians: {
        desc: "Converts an angle value in degrees to radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("RADIANS requires exactly 1 arguments.");
            }
            return formulajs.RADIANS.apply(null, values as [Value]);
        },
        completion: "RADIANS("
    },

    rand: {
        desc: "Returns a random number between 0 inclusive and 1 exclusive. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 0) {
                throw new Error("RAND requires exactly 0 arguments.");
            }
            return formulajs.RAND.apply(null, values as []);
        },
        completion: "RAND("
    },

    randbetween: {
        desc: "Returns a uniformly random integer between two values, inclusive. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("RANDBETWEEN requires exactly 2 arguments.");
            }
            return formulajs.RANDBETWEEN.apply(null, values as [Value, Value]);
        },
        completion: "RANDBETWEEN("
    },

    round: {
        desc: "Rounds a number to a certain number of decimal places according to standard rules. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("ROUND requires exactly 2 arguments.");
            }
            return formulajs.ROUND.apply(null, values as [Value, Value]);
        },
        completion: "ROUND("
    },

    rounddown: {
        desc: "Rounds a number to a certain number of decimal places, always rounding down to the next valid increment. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("ROUNDDOWN requires exactly 2 arguments.");
            }
            return formulajs.ROUNDDOWN.apply(null, values as [Value, Value]);
        },
        completion: "ROUNDDOWN("
    },

    roundup: {
        desc: "Rounds a number to a certain number of decimal places, always rounding up to the next valid increment. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("ROUNDUP requires exactly 2 arguments.");
            }
            return formulajs.ROUNDUP.apply(null, values as [Value, Value]);
        },
        completion: "ROUNDUP("
    },

    sec: {
        desc: "The SEC function returns the secant of an angle, measured in radians. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SEC requires exactly 1 arguments.");
            }
            return formulajs.SEC.apply(null, values as [Value]);
        },
        completion: "SEC("
    },

    sech: {
        desc: "The SECH function returns the hyperbolic secant of an angle. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SECH requires exactly 1 arguments.");
            }
            return formulajs.SECH.apply(null, values as [Value]);
        },
        completion: "SECH("
    },

    sign: {
        desc: "Given an input number, returns `-1` if it is negative, `1` if positive, and `0` if it is zero. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SIGN requires exactly 1 arguments.");
            }
            return formulajs.SIGN.apply(null, values as [Value]);
        },
        completion: "SIGN("
    },

    sin: {
        desc: "Returns the sine of an angle provided in radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SIN requires exactly 1 arguments.");
            }
            return formulajs.SIN.apply(null, values as [Value]);
        },
        completion: "SIN("
    },

    sinh: {
        desc: "Returns the hyperbolic sine of any real number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SINH requires exactly 1 arguments.");
            }
            return formulajs.SINH.apply(null, values as [Value]);
        },
        completion: "SINH("
    },

    sqrt: {
        desc: "Returns the positive square root of a positive number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SQRT requires exactly 1 arguments.");
            }
            return formulajs.SQRT.apply(null, values as [Value]);
        },
        completion: "SQRT("
    },

    sqrtpi: {
        desc: "Returns the positive square root of the product of Pi and the given positive number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SQRTPI requires exactly 1 arguments.");
            }
            return formulajs.SQRTPI.apply(null, values as [Value]);
        },
        completion: "SQRTPI("
    },

    subtotal: {
        desc: "Returns a subtotal for a vertical range of cells using a specified aggregation function. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("SUBTOTAL requires exactly 6 arguments.");
            }
            return formulajs.SUBTOTAL.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "SUBTOTAL("
    },

    sum: {
        desc: "Returns the sum of a series of numbers and/or cells. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("SUM requires exactly 4 arguments.");
            }
            return formulajs.SUM.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "SUM("
    },

    sumif: {
        desc: "Returns a conditional sum across a range. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("SUMIF requires exactly 2 arguments.");
            }
            return formulajs.SUMIF.apply(null, values as [Value, Value]);
        },
        completion: "SUMIF("
    },

    sumifs: {
        desc: "Returns the sum of a range depending on multiple criteria. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 11) {
                throw new Error("SUMIFS requires exactly 11 arguments.");
            }
            return formulajs.SUMIFS.apply(null, values as [Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "SUMIFS("
    },

    sumproduct: {
        desc: "Calculates the sum of the products of corresponding entries in two equal-sized arrays or ranges. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 7) {
                throw new Error("SUMPRODUCT requires exactly 7 arguments.");
            }
            return formulajs.SUMPRODUCT.apply(null, values as [Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "SUMPRODUCT("
    },

    sumsq: {
        desc: "Returns the sum of the squares of a series of numbers and/or cells. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("SUMSQ requires exactly 2 arguments.");
            }
            return formulajs.SUMSQ.apply(null, values as [Value, Value]);
        },
        completion: "SUMSQ("
    },

    sumx2my2: {
        desc: "Calculates the sum of the differences of the squares of values in two arrays. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("SUMX2MY2 requires exactly 3 arguments.");
            }
            return formulajs.SUMX2MY2.apply(null, values as [Value, Value, Value]);
        },
        completion: "SUMX2MY2("
    },

    sumx2py2: {
        desc: "Calculates the sum of the sums of the squares of values in two arrays. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("SUMX2PY2 requires exactly 3 arguments.");
            }
            return formulajs.SUMX2PY2.apply(null, values as [Value, Value, Value]);
        },
        completion: "SUMX2PY2("
    },

    sumxmy2: {
        desc: "Calculates the sum of the squares of differences of values in two arrays. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("SUMXMY2 requires exactly 3 arguments.");
            }
            return formulajs.SUMXMY2.apply(null, values as [Value, Value, Value]);
        },
        completion: "SUMXMY2("
    },

    tan: {
        desc: "Returns the tangent of an angle provided in radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("TAN requires exactly 1 arguments.");
            }
            return formulajs.TAN.apply(null, values as [Value]);
        },
        completion: "TAN("
    },

    tanh: {
        desc: "Returns the hyperbolic tangent of any real number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("TANH requires exactly 1 arguments.");
            }
            return formulajs.TANH.apply(null, values as [Value]);
        },
        completion: "TANH("
    },

    trunc: {
        desc: "Truncates a number to a certain number of significant digits by omitting less significant digits. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("TRUNC requires exactly 1 arguments.");
            }
            return formulajs.TRUNC.apply(null, values as [Value]);
        },
        completion: "TRUNC("
    },

    avedev: {
        desc: "Calculates the average of the magnitudes of deviations of data from a dataset\'s mean. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("AVEDEV requires exactly 3 arguments.");
            }
            return formulajs.AVEDEV.apply(null, values as [Value, Value, Value]);
        },
        completion: "AVEDEV("
    },

    average: {
        desc: "Returns the numerical average value in a dataset, ignoring text. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("AVERAGE requires exactly 3 arguments.");
            }
            return formulajs.AVERAGE.apply(null, values as [Value, Value, Value]);
        },
        completion: "AVERAGE("
    },

    averagea: {
        desc: "Returns the numerical average value in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("AVERAGEA requires exactly 3 arguments.");
            }
            return formulajs.AVERAGEA.apply(null, values as [Value, Value, Value]);
        },
        completion: "AVERAGEA("
    },

    averageif: {
        desc: "Returns the average of a range depending on criteria. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("AVERAGEIF requires exactly 6 arguments.");
            }
            return formulajs.AVERAGEIF.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "AVERAGEIF("
    },

    averageifs: {
        desc: "Returns the average of a range depending on multiple criteria. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 11) {
                throw new Error("AVERAGEIFS requires exactly 11 arguments.");
            }
            return formulajs.AVERAGEIFS.apply(null, values as [Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "AVERAGEIFS("
    },

    betadist: {
        desc: "See BETA.DIST.",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("BETADIST requires exactly 6 arguments.");
            }
            return formulajs.BETADIST.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "BETADIST("
    },

    betainv: {
        desc: "See BETA.INV",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("BETAINV requires exactly 5 arguments.");
            }
            return formulajs.BETAINV.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "BETAINV("
    },

    binomdist: {
        desc: "Calculates the probability of drawing a certain number of successes (or a maximum number of successes) in a certain number of tries given a population of a certain size containing a certain number of successes, with replacement of draws. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("BINOMDIST requires exactly 4 arguments.");
            }
            return formulajs.BINOMDIST.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "BINOMDIST("
    },

    correl: {
        desc: "Calculates r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("CORREL requires exactly 6 arguments.");
            }
            return formulajs.CORREL.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "CORREL("
    },

    count: {
        desc: "Returns a count of the number of numeric values in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("COUNT requires exactly 3 arguments.");
            }
            return formulajs.COUNT.apply(null, values as [Value, Value, Value]);
        },
        completion: "COUNT("
    },

    counta: {
        desc: "Returns a count of the number of values in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("COUNTA requires exactly 1 arguments.");
            }
            return formulajs.COUNTA.apply(null, values as [Value]);
        },
        completion: "COUNTA("
    },

    countblank: {
        desc: "Returns the number of empty cells in a given range. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("COUNTBLANK requires exactly 1 arguments.");
            }
            return formulajs.COUNTBLANK.apply(null, values as [Value]);
        },
        completion: "COUNTBLANK("
    },

    countif: {
        desc: "Returns a conditional count across a range. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("COUNTIF requires exactly 2 arguments.");
            }
            return formulajs.COUNTIF.apply(null, values as [Value, Value]);
        },
        completion: "COUNTIF("
    },

    countifs: {
        desc: "Returns the count of a range depending on multiple criteria. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 11) {
                throw new Error("COUNTIFS requires exactly 11 arguments.");
            }
            return formulajs.COUNTIFS.apply(null, values as [Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "COUNTIFS("
    },

    countunique: {
        desc: "Counts the number of unique values in a list of specified values and ranges. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("COUNTUNIQUE requires exactly 1 arguments.");
            }
            return formulajs.COUNTUNIQUE.apply(null, values as [Value]);
        },
        completion: "COUNTUNIQUE("
    },

    devsq: {
        desc: "Calculates the sum of squares of deviations based on a sample. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DEVSQ requires exactly 1 arguments.");
            }
            return formulajs.DEVSQ.apply(null, values as [Value]);
        },
        completion: "DEVSQ("
    },

    expondist: {
        desc: "See EXPON.DIST",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("EXPONDIST requires exactly 3 arguments.");
            }
            return formulajs.EXPONDIST.apply(null, values as [Value, Value, Value]);
        },
        completion: "EXPONDIST("
    },

    fdist: {
        desc: "See F.DIST.RT.",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("FDIST requires exactly 4 arguments.");
            }
            return formulajs.FDIST.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "FDIST("
    },

    finv: {
        desc: "See F.INV.RT",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("FINV requires exactly 3 arguments.");
            }
            return formulajs.FINV.apply(null, values as [Value, Value, Value]);
        },
        completion: "FINV("
    },

    fisher: {
        desc: "Returns the Fisher transformation of a specified value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("FISHER requires exactly 1 arguments.");
            }
            return formulajs.FISHER.apply(null, values as [Value]);
        },
        completion: "FISHER("
    },

    fisherinv: {
        desc: "Returns the inverse Fisher transformation of a specified value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("FISHERINV requires exactly 1 arguments.");
            }
            return formulajs.FISHERINV.apply(null, values as [Value]);
        },
        completion: "FISHERINV("
    },

    forecast: {
        desc: "Calculates the expected y-value for a specified x based on a linear regression of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 11) {
                throw new Error("FORECAST requires exactly 11 arguments.");
            }
            return formulajs.FORECAST.apply(null, values as [Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "FORECAST("
    },

    frequency: {
        desc: "Calculates the frequency distribution of a one-column array into specified classes. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("FREQUENCY requires exactly 4 arguments.");
            }
            return formulajs.FREQUENCY.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "FREQUENCY("
    },

    gamma: {
        desc: "Returns the Gamma function evaluated at the specified value. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("GAMMA requires exactly 1 arguments.");
            }
            return formulajs.GAMMA.apply(null, values as [Value]);
        },
        completion: "GAMMA("
    },

    gammaln: {
        desc: "Returns the the logarithm of a specified Gamma function, base e (Euler\'s number). ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("GAMMALN requires exactly 1 arguments.");
            }
            return formulajs.GAMMALN.apply(null, values as [Value]);
        },
        completion: "GAMMALN("
    },

    gauss: {
        desc: "The GAUSS function returns the probability that a random variable, drawn from a normal distribution, will be between the mean and z standard deviations above (or below) the mean. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("GAUSS requires exactly 1 arguments.");
            }
            return formulajs.GAUSS.apply(null, values as [Value]);
        },
        completion: "GAUSS("
    },

    geomean: {
        desc: "Calculates the geometric mean of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("GEOMEAN requires exactly 3 arguments.");
            }
            return formulajs.GEOMEAN.apply(null, values as [Value, Value, Value]);
        },
        completion: "GEOMEAN("
    },

    growth: {
        desc: "Given partial data about an exponential growth trend, fits an ideal exponential growth trend and/or predicts further values. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("GROWTH requires exactly 6 arguments.");
            }
            return formulajs.GROWTH.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "GROWTH("
    },

    harmean: {
        desc: "Calculates the harmonic mean of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("HARMEAN requires exactly 3 arguments.");
            }
            return formulajs.HARMEAN.apply(null, values as [Value, Value, Value]);
        },
        completion: "HARMEAN("
    },

    hypgeomdist: {
        desc: "Calculates the probability of drawing a certain number of successes in a certain number of tries given a population of a certain size containing a certain number of successes, without replacement of draws. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("HYPGEOMDIST requires exactly 5 arguments.");
            }
            return formulajs.HYPGEOMDIST.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "HYPGEOMDIST("
    },

    intercept: {
        desc: "Calculates the y-value at which the line resulting from linear regression of a dataset will intersect the y-axis (x=0). ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("INTERCEPT requires exactly 6 arguments.");
            }
            return formulajs.INTERCEPT.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "INTERCEPT("
    },

    kurt: {
        desc: "Calculates the kurtosis of a dataset, which describes the shape, and in particular the \"peakedness\" of that dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("KURT requires exactly 1 arguments.");
            }
            return formulajs.KURT.apply(null, values as [Value]);
        },
        completion: "KURT("
    },

    large: {
        desc: "Returns the nth largest element from a data set, where n is user-defined. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("LARGE requires exactly 2 arguments.");
            }
            return formulajs.LARGE.apply(null, values as [Value, Value]);
        },
        completion: "LARGE("
    },

    linest: {
        desc: "Given partial data about a linear trend, calculates various parameters about the ideal linear trend using the least-squares method. ",
        compute: (values: Array<Value>) : Value => {
            return formulajs.LINEST.apply(null, values as [Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "LINEST("
    },

    lognormdist: {
        desc: "Returns the value of the log-normal cumulative distribution with given mean and standard deviation at a specified value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("LOGNORMDIST requires exactly 4 arguments.");
            }
            return formulajs.LOGNORMDIST.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "LOGNORMDIST("
    },

    max: {
        desc: "Returns the maximum value in a numeric dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("MAX requires exactly 5 arguments.");
            }
            return formulajs.MAX.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "MAX("
    },

    maxa: {
        desc: "Returns the maximum numeric value in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("MAXA requires exactly 5 arguments.");
            }
            return formulajs.MAXA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "MAXA("
    },

    median: {
        desc: "Returns the median value in a numeric dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("MEDIAN requires exactly 4 arguments.");
            }
            return formulajs.MEDIAN.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "MEDIAN("
    },

    min: {
        desc: "Returns the minimum value in a numeric dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("MIN requires exactly 5 arguments.");
            }
            return formulajs.MIN.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "MIN("
    },

    mina: {
        desc: "Returns the minimum numeric value in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("MINA requires exactly 5 arguments.");
            }
            return formulajs.MINA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "MINA("
    },

    normdist: {
        desc: "Returns the value of the normal distribution function (or normal cumulative distribution function) for a specified value, mean, and standard deviation. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("NORMDIST requires exactly 4 arguments.");
            }
            return formulajs.NORMDIST.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "NORMDIST("
    },

    norminv: {
        desc: "Returns the value of the inverse normal distribution function for a specified value, mean, and standard deviation. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("NORMINV requires exactly 3 arguments.");
            }
            return formulajs.NORMINV.apply(null, values as [Value, Value, Value]);
        },
        completion: "NORMINV("
    },

    normsdist: {
        desc: "Returns the value of the standard normal cumulative distribution function for a specified value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("NORMSDIST requires exactly 2 arguments.");
            }
            return formulajs.NORMSDIST.apply(null, values as [Value, Value]);
        },
        completion: "NORMSDIST("
    },

    normsinv: {
        desc: "Returns the value of the inverse standard normal distribution function for a specified value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("NORMSINV requires exactly 1 arguments.");
            }
            return formulajs.NORMSINV.apply(null, values as [Value]);
        },
        completion: "NORMSINV("
    },

    pearson: {
        desc: "Calculates r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("PEARSON requires exactly 6 arguments.");
            }
            return formulajs.PEARSON.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "PEARSON("
    },

    permut: {
        desc: "Returns the number of ways to choose some number of objects from a pool of a given size of objects, considering order. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("PERMUT requires exactly 2 arguments.");
            }
            return formulajs.PERMUT.apply(null, values as [Value, Value]);
        },
        completion: "PERMUT("
    },

    permutationa: {
        desc: "Returns the number of permutations for selecting a group of objects (with replacement) from a total number of objects. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("PERMUTATIONA requires exactly 2 arguments.");
            }
            return formulajs.PERMUTATIONA.apply(null, values as [Value, Value]);
        },
        completion: "PERMUTATIONA("
    },

    phi: {
        desc: "The PHI function returns the value of the normal distribution with mean 0 and standard deviation 1. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("PHI requires exactly 1 arguments.");
            }
            return formulajs.PHI.apply(null, values as [Value]);
        },
        completion: "PHI("
    },

    prob: {
        desc: "Given a set of values and corresponding probabilities, calculates the probability that a value chosen at random falls between two limits. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 7) {
                throw new Error("PROB requires exactly 7 arguments.");
            }
            return formulajs.PROB.apply(null, values as [Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "PROB("
    },

    rsq: {
        desc: "Calculates the square of r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("RSQ requires exactly 6 arguments.");
            }
            return formulajs.RSQ.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "RSQ("
    },

    skew: {
        desc: "Calculates the skewness of a dataset, which describes the symmetry of that dataset about the mean. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SKEW requires exactly 1 arguments.");
            }
            return formulajs.SKEW.apply(null, values as [Value]);
        },
        completion: "SKEW("
    },

    slope: {
        desc: "Calculates the slope of the line resulting from linear regression of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("SLOPE requires exactly 5 arguments.");
            }
            return formulajs.SLOPE.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "SLOPE("
    },

    small: {
        desc: "Returns the nth smallest element from a data set, where n is user-defined. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("SMALL requires exactly 2 arguments.");
            }
            return formulajs.SMALL.apply(null, values as [Value, Value]);
        },
        completion: "SMALL("
    },

    standardize: {
        desc: "Calculates the normalized equivalent of a random variable given mean and standard deviation of the distribution. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("STANDARDIZE requires exactly 3 arguments.");
            }
            return formulajs.STANDARDIZE.apply(null, values as [Value, Value, Value]);
        },
        completion: "STANDARDIZE("
    },

    stdeva: {
        desc: "Calculates the standard deviation based on a sample, setting text to the value `0`. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("STDEVA requires exactly 5 arguments.");
            }
            return formulajs.STDEVA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "STDEVA("
    },

    stdevp: {
        desc: "Calculates the standard deviation based on an entire population. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("STDEVP requires exactly 5 arguments.");
            }
            return formulajs.STDEVP.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "STDEVP("
    },

    stdevpa: {
        desc: "Calculates the standard deviation based on an entire population, setting text to the value `0`. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("STDEVPA requires exactly 5 arguments.");
            }
            return formulajs.STDEVPA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "STDEVPA("
    },

    steyx: {
        desc: "Calculates the standard error of the predicted y-value for each x in the regression of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 8) {
                throw new Error("STEYX requires exactly 8 arguments.");
            }
            return formulajs.STEYX.apply(null, values as [Value, Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "STEYX("
    },

    tdist: {
        desc: "Calculates the probability for Student\'s t-distribution with a given input (x). ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("TDIST requires exactly 3 arguments.");
            }
            return formulajs.TDIST.apply(null, values as [Value, Value, Value]);
        },
        completion: "TDIST("
    },

    tinv: {
        desc: "See T.INV.2T",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("TINV requires exactly 2 arguments.");
            }
            return formulajs.TINV.apply(null, values as [Value, Value]);
        },
        completion: "TINV("
    },

    trimmean: {
        desc: "Calculates the mean of a dataset excluding some proportion of data from the high and low ends of the dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("TRIMMEAN requires exactly 2 arguments.");
            }
            return formulajs.TRIMMEAN.apply(null, values as [Value, Value]);
        },
        completion: "TRIMMEAN("
    },

    vara: {
        desc: "Calculates an estimate of variance based on a sample, setting text to the value `0`. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("VARA requires exactly 5 arguments.");
            }
            return formulajs.VARA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "VARA("
    },

    varp: {
        desc: "Calculates the variance based on an entire population. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("VARP requires exactly 5 arguments.");
            }
            return formulajs.VARP.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "VARP("
    },

    varpa: {
        desc: "Calculates the variance based on an entire population, setting text to the value `0`. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 5) {
                throw new Error("VARPA requires exactly 5 arguments.");
            }
            return formulajs.VARPA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "VARPA("
    },

    ztest: {
        desc: "See Z.TEST.",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("ZTEST requires exactly 2 arguments.");
            }
            return formulajs.ZTEST.apply(null, values as [Value, Value]);
        },
        completion: "ZTEST("
    },

    char: {
        desc: "Convert a number into a character according to the current Unicode table. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("CHAR requires exactly 1 arguments.");
            }
            return formulajs.CHAR.apply(null, values as [Value]);
        },
        completion: "CHAR("
    },

    clean: {
        desc: "Returns the text with the non-printable ASCII characters removed. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("CLEAN requires exactly 1 arguments.");
            }
            return formulajs.CLEAN.apply(null, values as [Value]);
        },
        completion: "CLEAN("
    },

    code: {
        desc: "Returns the numeric Unicode map value of the first character in the string provided. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("CODE requires exactly 1 arguments.");
            }
            return formulajs.CODE.apply(null, values as [Value]);
        },
        completion: "CODE("
    },

    concatenate: {
        desc: "Appends strings to one another. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("CONCATENATE requires exactly 3 arguments.");
            }
            return formulajs.CONCATENATE.apply(null, values as [Value, Value, Value]);
        },
        completion: "CONCATENATE("
    },

    exact: {
        desc: "Tests whether two strings are identical. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("EXACT requires exactly 2 arguments.");
            }
            return formulajs.EXACT.apply(null, values as [Value, Value]);
        },
        completion: "EXACT("
    },

    find: {
        desc: "Returns the position at which a string is first found within text. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("FIND requires exactly 3 arguments.");
            }
            return formulajs.FIND.apply(null, values as [Value, Value, Value]);
        },
        completion: "FIND("
    },

    left: {
        desc: "Returns a substring from the beginning of a specified string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("LEFT requires exactly 2 arguments.");
            }
            return formulajs.LEFT.apply(null, values as [Value, Value]);
        },
        completion: "LEFT("
    },

    len: {
        desc: "Returns the length of a string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("LEN requires exactly 1 arguments.");
            }
            return formulajs.LEN.apply(null, values as [Value]);
        },
        completion: "LEN("
    },

    lower: {
        desc: "Converts a specified string to lowercase. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("LOWER requires exactly 1 arguments.");
            }
            return formulajs.LOWER.apply(null, values as [Value]);
        },
        completion: "LOWER("
    },

    mid: {
        desc: "Returns a segment of a string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("MID requires exactly 3 arguments.");
            }
            return formulajs.MID.apply(null, values as [Value, Value, Value]);
        },
        completion: "MID("
    },

    proper: {
        desc: "Capitalizes each word in a specified string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("PROPER requires exactly 1 arguments.");
            }
            return formulajs.PROPER.apply(null, values as [Value]);
        },
        completion: "PROPER("
    },

    regexextract: {
        desc: "Extracts matching substrings according to a regular expression. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("REGEXEXTRACT requires exactly 2 arguments.");
            }
            return formulajs.REGEXEXTRACT.apply(null, values as [Value, Value]);
        },
        completion: "REGEXEXTRACT("
    },

    regexmatch: {
        desc: "Whether a piece of text matches a regular expression. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("REGEXMATCH requires exactly 2 arguments.");
            }
            return formulajs.REGEXMATCH.apply(null, values as [Value, Value]);
        },
        completion: "REGEXMATCH("
    },

    regexreplace: {
        desc: "Replaces part of a text string with a different text string using regular expressions. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("REGEXREPLACE requires exactly 3 arguments.");
            }
            return formulajs.REGEXREPLACE.apply(null, values as [Value, Value, Value]);
        },
        completion: "REGEXREPLACE("
    },

    replace: {
        desc: "Replaces part of a text string with a different text string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("REPLACE requires exactly 4 arguments.");
            }
            return formulajs.REPLACE.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "REPLACE("
    },

    rept: {
        desc: "Returns specified text repeated a number of times. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("REPT requires exactly 2 arguments.");
            }
            return formulajs.REPT.apply(null, values as [Value, Value]);
        },
        completion: "REPT("
    },

    right: {
        desc: "Returns a substring from the end of a specified string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("RIGHT requires exactly 2 arguments.");
            }
            return formulajs.RIGHT.apply(null, values as [Value, Value]);
        },
        completion: "RIGHT("
    },

    roman: {
        desc: "Formats a number in Roman numerals. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ROMAN requires exactly 1 arguments.");
            }
            return formulajs.ROMAN.apply(null, values as [Value]);
        },
        completion: "ROMAN("
    },

    search: {
        desc: "Returns the position at which a string is first found within text. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("SEARCH requires exactly 2 arguments.");
            }
            return formulajs.SEARCH.apply(null, values as [Value, Value]);
        },
        completion: "SEARCH("
    },

    split: {
        desc: "Divides text around a specified character or string, and puts each fragment into a separate cell in the row. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("SPLIT requires exactly 3 arguments.");
            }
            return formulajs.SPLIT.apply(null, values as [Value, Value, Value]);
        },
        completion: "SPLIT("
    },

    substitute: {
        desc: "Replaces existing text with new text in a string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("SUBSTITUTE requires exactly 4 arguments.");
            }
            return formulajs.SUBSTITUTE.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "SUBSTITUTE("
    },

    t: {
        desc: "Returns string arguments as text. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("T requires exactly 1 arguments.");
            }
            return formulajs.T.apply(null, values as [Value]);
        },
        completion: "T("
    },

    trim: {
        desc: "Removes leading and trailing spaces in a specified string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("TRIM requires exactly 1 arguments.");
            }
            return formulajs.TRIM.apply(null, values as [Value]);
        },
        completion: "TRIM("
    },

    textjoin: {
        desc: "Combines the text from multiple strings and/or arrays, with a specifiable delimiter separating the different texts. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 9) {
                throw new Error("TEXTJOIN requires exactly 9 arguments.");
            }
            return formulajs.TEXTJOIN.apply(null, values as [Value, Value, Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "TEXTJOIN("
    },

    unichar: {
        desc: "Returns the Unicode character for a number. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("UNICHAR requires exactly 1 arguments.");
            }
            return formulajs.UNICHAR.apply(null, values as [Value]);
        },
        completion: "UNICHAR("
    },

    unicode: {
        desc: "Returns the decimal Unicode value of the first character of the text. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("UNICODE requires exactly 1 arguments.");
            }
            return formulajs.UNICODE.apply(null, values as [Value]);
        },
        completion: "UNICODE("
    },

    upper: {
        desc: "Converts a specified string to uppercase. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("UPPER requires exactly 1 arguments.");
            }
            return formulajs.UPPER.apply(null, values as [Value]);
        },
        completion: "UPPER("
    },
};
