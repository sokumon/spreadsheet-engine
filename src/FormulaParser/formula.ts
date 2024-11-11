import * as formulajs from '@formulajs/formulajs';

import { Value } from './defination';
export const formulas = {

    date: {
        desc: "Converts a provided year, month, and day into a date. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("DATE requires exactly 3 arguments.");
            }
            // Call formulajs.DATE using apply
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
            // Call formulajs.DATEVALUE using apply
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
            // Call formulajs.DAY using apply
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
            // Call formulajs.DAYS using apply
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
            // Call formulajs.DAYS360 using apply
            return formulajs.DAYS360.apply(null, values as [Value, Value,Value]);
        },
        completion: "DAYS360("
    },

    edate: {
        desc: "Returns a date a specified number of months before or after another date. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("EDATE requires exactly 2 arguments.");
            }
            // Call formulajs.EDATE using apply
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
            // Call formulajs.EOMONTH using apply
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
            // Call formulajs.HOUR using apply
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
            // Call formulajs.MINUTE using apply
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
            // Call formulajs.ISOWEEKNUM using apply
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
            // Call formulajs.MONTH using apply
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
            // Call formulajs.NETWORKDAYS using apply
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
            // Call formulajs.NOW using apply
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
            // Call formulajs.SECOND using apply
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
            // Call formulajs.TIME using apply
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
            // Call formulajs.TIMEVALUE using apply
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
            // Call formulajs.TODAY using apply
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
            // Call formulajs.WEEKDAY using apply
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
            // Call formulajs.YEAR using apply
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
            // Call formulajs.WEEKNUM using apply
            return formulajs.WEEKNUM.apply(null, values as [Value, Value]);
        },
        completion: "WEEKNUM("
    },

    workday: {
        desc: "Calculates the end date after a specified number of working days. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.WORKDAY using apply
            return formulajs.WORKDAY.apply(null, values as [Value, Value, Value,]);
        },
        completion: "WORKDAY("
    },

    workdayintl: {
        desc: "Calculates the end date after a specified number of working days. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("WORKDAYINTL d requires exactly 3 arguments.");
            }
            // Call formulajs.WORKDAY using apply
            return formulajs.WORKDAYINTL.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "WORKDAY("
    },

    yearfrac: {
        desc: "Returns the number of years, including fractional years, between two dates using a specified day count convention. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("YEARFRAC requires exactly 3 arguments.");
            }
            // Call formulajs.YEARFRAC using apply
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
            // Call formulajs.ACCRINT using apply
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
            // Call formulajs.CUMIPMT using apply
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
            // Call formulajs.CUMPRINC using apply
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
            // Call formulajs.DB using apply
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
            // Call formulajs.DDB using apply
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
            // Call formulajs.DOLLARDE using apply
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
            // Call formulajs.DOLLARFR using apply
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
            // Call formulajs.EFFECT using apply
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
            // Call formulajs.FV using apply
            return formulajs.FV.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "FV("
    },

    fvschedule: {
        desc: "Calculates the future value of some principal based on a specified series of potentially varying interest rates. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.FVSCHEDULE using apply
            return formulajs.FVSCHEDULE.apply(null, values as [Value, Value]);
        },
        completion: "FVSCHEDULE("
    },

    ipmt: {
        desc: "Calculates the payment on interest for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 6) {
                throw new Error("IPMT requires exactly 6 arguments.");
            }
            // Call formulajs.IPMT using apply
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
            // Call formulajs.IRR using apply
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
            // Call formulajs.ISPMT using apply
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
            // Call formulajs.MIRR using apply
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
            // Call formulajs.NOMINAL using apply
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
            // Call formulajs.NPER using apply
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
            // Call formulajs.NPV using apply
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
            // Call formulajs.PDURATION using apply
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
            // Call formulajs.PMT using apply
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
            // Call formulajs.PPMT using apply
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
            // Call formulajs.PV using apply
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
            // Call formulajs.RATE using apply
            return formulajs.RATE.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "RATE("
    },

    bintodec: {
        desc: "Converts a signed binary number to decimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("BIN2DEC requires exactly 1 arguments.");
            }
            // Call formulajs.BIN2DEC using apply
            return formulajs.BIN2DEC.apply(null, values as [Value]);
        },
        completion: "BIN2DEC("
    },

    bintohex: {
        desc: "Converts a signed binary number to signed hexadecimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("BINTOHEX requires exactly 1 arguments.");
            }
            // Call formulajs.BINTOHEX using apply
            return formulajs.BIN2HEX.apply(null, values as [Value,Value]);
        },
        completion: "BINTOHEX("
    },

    bintooct: {
        desc: "Converts a signed binary number to signed octal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("BINTOOCT requires exactly 1 arguments.");
            }
            // Call formulajs.BINTOOCT using apply
            return formulajs.BIN2OCT.apply(null, values as [Value,Value]);
        },
        completion: "BINTOOCT("
    },

    bitand: {
        desc: "Bitwise boolean AND of two numbers. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("BITAND requires exactly 2 arguments.");
            }
            // Call formulajs.BITAND using apply
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
            // Call formulajs.BITLSHIFT using apply
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
            // Call formulajs.BITOR using apply
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
            // Call formulajs.BITRSHIFT using apply
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
            // Call formulajs.BITXOR using apply
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
            // Call formulajs.COMPLEX using apply
            return formulajs.COMPLEX.apply(null, values as [Value, Value,Value]);
        },
        completion: "COMPLEX("
    },

    convert: {
        desc: "Converts a numeric value to a different unit of measure. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("CONVERT requires exactly 3 arguments.");
            }
            // Call formulajs.CONVERT using apply
            return formulajs.CONVERT.apply(null, values as [Value, Value, Value]);
        },
        completion: "CONVERT("
    },

    dectobin: {
        desc: "Converts a decimal number to signed binary format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DECtoBIN requires exactly 1 arguments.");
            }
            // Call formulajs.DEC2BIN using apply
            return formulajs.DEC2BIN.apply(null, values as [Value,Value]);
        },
        completion: "DECtoBIN("
    },

    dectohex: {
        desc: "Converts a decimal number to signed hexadecimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DECtoHEX requires exactly 1 arguments.");
            }
            // Call formulajs.DEC2HEX using apply
            return formulajs.DEC2HEX.apply(null, values as [Value,Value]);
        },
        completion: "DECtoHEX("
    },

    dectooct: {
        desc: "Converts a decimal number to signed octal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DECTOOCT requires exactly 1 arguments.");
            }
            // Call formulajs.DECTOOCT using apply
            return formulajs.DEC2OCT.apply(null, values as [Value,Value]);
        },
        completion: "DECTOOCT("
    },

    delta: {
        desc: "Compare two numeric values, returning 1 if they\'re equal. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("DELTA requires exactly 2 arguments.");
            }
            // Call formulajs.DELTA using apply
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
            // Call formulajs.ERF using apply
            return formulajs.ERF.apply(null, values as [Value,Value]);
        },
        completion: "ERF("
    },

    erfc: {
        desc: "Returns the complementary Gauss error function of a value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("ERFC requires exactly 1 arguments.");
            }
            // Call formulajs.ERFC using apply
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
            // Call formulajs.GESTEP using apply
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
            // Call formulajs.HEX2BIN using apply
            return formulajs.HEX2BIN.apply(null, values as [Value,Value]);
        },
        completion: "HEX2BIN("
    },

    hex2dec: {
        desc: "Converts a signed hexadecimal number to decimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("HEX2DEC requires exactly 1 arguments.");
            }
            // Call formulajs.HEX2DEC using apply
            return formulajs.HEX2DEC.apply(null, values as [Value]);
        },
        completion: "HEX2DEC("
    },

    hex2oct: {
        desc: "Converts a signed hexadecimal number to signed octal format. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.HEX2OCT using apply
            return formulajs.HEX2OCT.apply(null, values as [Value,Value]);
        },
        completion: "HEX2OCT("
    },

    imabs: {
        desc: "Returns absolute value of a complex number. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("IMABS requires exactly 1 arguments.");
            }
            // Call formulajs.IMABS using apply
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
            // Call formulajs.IMAGINARY using apply
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
            // Call formulajs.IMARGUMENT using apply
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
            // Call formulajs.IMCONJUGATE using apply
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
            // Call formulajs.IMCOS using apply
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
            // Call formulajs.IMCOSH using apply
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
            // Call formulajs.IMCOT using apply
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
            // Call formulajs.IMCSC using apply
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
            // Call formulajs.IMCSCH using apply
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
            // Call formulajs.IMDIV using apply
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
            // Call formulajs.IMEXP using apply
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
            // Call formulajs.IMLN using apply
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
            // Call formulajs.IMLOG10 using apply
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
            // Call formulajs.IMLOG2 using apply
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
            // Call formulajs.IMPOWER using apply
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
            // Call formulajs.IMPRODUCT using apply
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
            // Call formulajs.IMREAL using apply
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
            // Call formulajs.IMSEC using apply
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
            // Call formulajs.IMSECH using apply
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
            // Call formulajs.IMSIN using apply
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
            // Call formulajs.IMSINH using apply
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
            // Call formulajs.IMSQRT using apply
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
            // Call formulajs.IMSUB using apply
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
            // Call formulajs.IMSUM using apply
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
            // Call formulajs.IMTAN using apply
            return formulajs.IMTAN.apply(null, values as [Value]);
        },
        completion: "IMTAN("
    },

    oct2bin: {
        desc: "Converts a signed octal number to signed binary format. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.OCT2BIN using apply
            return formulajs.OCT2BIN.apply(null, values as [Value,Value]);
        },
        completion: "OCT2BIN("
    },

    oct2dec: {
        desc: "Converts a signed octal number to decimal format. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("OCT2DEC requires exactly 1 arguments.");
            }
            // Call formulajs.OCT2DEC using apply
            return formulajs.OCT2DEC.apply(null, values as [Value]);
        },
        completion: "OCT2DEC("
    },

    oct2hex: {
        desc: "Converts a signed octal number to signed hexadecimal format. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.OCT2HEX using apply
            return formulajs.OCT2HEX.apply(null, values as [Value,Value]);
        },
        completion: "OCT2HEX("
    },

    and: {
        desc: "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("AND requires exactly 3 arguments.");
            }
            // Call formulajs.AND using apply
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
            // Call formulajs.IF using apply
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
            // Call formulajs.IFS using apply
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
            // Call formulajs.IFERROR using apply
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
            // Call formulajs.IFNA using apply
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
            // Call formulajs.NOT using apply
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
            // Call formulajs.OR using apply
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
            // Call formulajs.SWITCH using apply
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
            // Call formulajs.XOR using apply
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
            // Call formulajs.ABS using apply
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
            // Call formulajs.ACOS using apply
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
            // Call formulajs.ACOSH using apply
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
            // Call formulajs.ACOT using apply
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
            // Call formulajs.ACOTH using apply
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
            // Call formulajs.ARABIC using apply
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
            // Call formulajs.ASIN using apply
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
            // Call formulajs.ASINH using apply
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
            // Call formulajs.ATAN using apply
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
            // Call formulajs.ATAN2 using apply
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
            // Call formulajs.ATANH using apply
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
            // Call formulajs.BASE using apply
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
            // Call formulajs.CEILING using apply
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
            // Call formulajs.COMBIN using apply
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
            // Call formulajs.COMBINA using apply
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
            // Call formulajs.COS using apply
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
            // Call formulajs.COSH using apply
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
            // Call formulajs.COT using apply
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
            // Call formulajs.COTH using apply
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
            // Call formulajs.CSC using apply
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
            // Call formulajs.CSCH using apply
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
            // Call formulajs.DECIMAL using apply
            return formulajs.DECIMAL.apply(null, values as [Value, Value]);
        },
        completion: "DECIMAL("
    },
    // Duplicate 
    // erf: {
    //     desc: "The ERF function returns the integral of the Gauss error function over an interval of values. .",
    //     compute: (values: Array<Value>) : Value => {
    //         if (values.length !== 1) {
    //             throw new Error("ERF requires exactly 1 arguments.");
    //         }
    //         // Call formulajs.ERF using apply
    //         return formulajs.ERF.apply(null, values as [Value,Value]);
    //     },
    //     completion: "ERF("
    // },

    // erfc: {
    //     desc: "Returns the complementary Gauss error function of a value. ",
    //     compute: (values: Array<Value>) : Value => {
    //         if (values.length !== 1) {
    //             throw new Error("ERFC requires exactly 1 arguments.");
    //         }
    //         // Call formulajs.ERFC using apply
    //         return formulajs.ERFC.apply(null, values as [Value]);
    //     },
    //     completion: "ERFC("
    // },

    even: {
        desc: "Rounds a number up to the nearest even integer. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("EVEN requires exactly 1 arguments.");
            }
            // Call formulajs.EVEN using apply
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
            // Call formulajs.EXP using apply
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
            // Call formulajs.FACT using apply
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
            // Call formulajs.FACTDOUBLE using apply
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
            // Call formulajs.FLOOR using apply
            return formulajs.FLOOR.apply(null, values as [Value,Value]);
        },
        completion: "FLOOR("
    },

    gcd: {
        desc: "Returns the greatest common divisor of one or more integers. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("GCD requires exactly 3 arguments.");
            }
            // Call formulajs.GCD using apply
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
            // Call formulajs.INT using apply
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
            // Call formulajs.ISEVEN using apply
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
            // Call formulajs.ISODD using apply
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
            // Call formulajs.LCM using apply
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
            // Call formulajs.LN using apply
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
            // Call formulajs.LOG using apply
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
            // Call formulajs.LOG10 using apply
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
            // Call formulajs.MOD using apply
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
            // Call formulajs.MROUND using apply
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
            // Call formulajs.MULTINOMIAL using apply
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
            // Call formulajs.ODD using apply
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
            // Call formulajs.POWER using apply
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
            // Call formulajs.PRODUCT using apply
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
            // Call formulajs.QUOTIENT using apply
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
            // Call formulajs.RADIANS using apply
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
            // Call formulajs.RAND using apply
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
            // Call formulajs.RANDBETWEEN using apply
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
            // Call formulajs.ROUND using apply
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
            // Call formulajs.ROUNDDOWN using apply
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
            // Call formulajs.ROUNDUP using apply
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
            // Call formulajs.SEC using apply
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
            // Call formulajs.SECH using apply
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
            // Call formulajs.SIGN using apply
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
            // Call formulajs.SIN using apply
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
            // Call formulajs.SINH using apply
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
            // Call formulajs.SQRT using apply
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
            // Call formulajs.SQRTPI using apply
            return formulajs.SQRTPI.apply(null, values as [Value]);
        },
        completion: "SQRTPI("
    },

    subtotal: {
        desc: "Returns a subtotal for a vertical range of cells using a specified aggregation function. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.SUBTOTAL using apply
            return formulajs.SUBTOTAL.apply(null, values as [Value, Value]);
        },
        completion: "SUBTOTAL("
    },

    sum: {
        desc: "Returns the sum of a series of numbers and/or cells. ",
        compute: (values: Array<Value>) : Value => {
            // if (values.length !== 4) {
            //     throw new Error("SUM requires exactly 4 arguments.");
            // }
            // Call formulajs.SUM using apply
            return formulajs.SUM.apply(null, values );
        },
        completion: "SUM("
    },

    sumif: {
        desc: "Returns a conditional sum across a range. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("SUMIF requires exactly 2 arguments.");
            }
            // Call formulajs.SUMIF using apply
            return formulajs.SUMIF.apply(null, values as [Value, Value, Value]);
        },
        completion: "SUMIF("
    },

    sumifs: {
        desc: "Returns the sum of a range depending on multiple criteria. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.SUMIFS using apply
            return formulajs.SUMIFS.apply(null, values as [Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "SUMIFS("
    },

    sumproduct: {
        desc: "Calculates the sum of the products of corresponding entries in two equal-sized arrays or ranges. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.SUMPRODUCT using apply
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
            // Call formulajs.SUMSQ using apply
            return formulajs.SUMSQ.apply(null, values as [Value, Value]);
        },
        completion: "SUMSQ("
    },

    sumx2my2: {
        desc: "Calculates the sum of the differences of the squares of values in two arrays. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.SUMX2MY2 using apply
            return formulajs.SUMX2MY2.apply(null, values as [Value, Value]);
        },
        completion: "SUMX2MY2("
    },

    sumx2py2: {
        desc: "Calculates the sum of the sums of the squares of values in two arrays. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.SUMX2PY2 using apply
            return formulajs.SUMX2PY2.apply(null, values as [Value, Value]);
        },
        completion: "SUMX2PY2("
    },

    sumxmy2: {
        desc: "Calculates the sum of the squares of differences of values in two arrays. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.SUMXMY2 using apply
            return formulajs.SUMXMY2.apply(null, values as [Value, Value]);
        },
        completion: "SUMXMY2("
    },

    tan: {
        desc: "Returns the tangent of an angle provided in radians. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("TAN requires exactly 1 arguments.");
            }
            // Call formulajs.TAN using apply
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
            // Call formulajs.TANH using apply
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
            // Call formulajs.TRUNC using apply
            return formulajs.TRUNC.apply(null, values as [Value,Value]);
        },
        completion: "TRUNC("
    },

    avedev: {
        desc: "Calculates the average of the magnitudes of deviations of data from a dataset\'s mean. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.AVEDEV using apply
            return formulajs.AVEDEV.apply(null, values as [Value, Value, Value]);
        },
        completion: "AVEDEV("
    },

    average: {
        desc: "Returns the numerical average value in a dataset, ignoring text. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.AVERAGE using apply
            return formulajs.AVERAGE.apply(null, values as [Value, Value, Value]);
        },
        completion: "AVERAGE("
    },

    averagea: {
        desc: "Returns the numerical average value in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.AVERAGEA using apply
            return formulajs.AVERAGEA.apply(null, values as [Value, Value, Value]);
        },
        completion: "AVERAGEA("
    },

    averageif: {
        desc: "Returns the average of a range depending on criteria. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.AVERAGEIF using apply
            return formulajs.AVERAGEIF.apply(null, values as [Value, Value, Value, Value, Value, Value]);
        },
        completion: "AVERAGEIF("
    },

    averageifs: {
        desc: "Returns the average of a range depending on multiple criteria. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.AVERAGEIFS using apply
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
            // Call formulajs.BETADIST using apply
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
            // Call formulajs.BETAINV using apply
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
            // Call formulajs.BINOMDIST using apply
            return formulajs.BINOMDIST.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "BINOMDIST("
    },

    correl: {
        desc: "Calculates r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.CORREL using apply
            return formulajs.CORREL.apply(null, values as [Value, Value]);
        },
        completion: "CORREL("
    },

    count: {
        desc: "Returns a count of the number of numeric values in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.COUNT using apply
            return formulajs.COUNT.apply(null, values as [Value, Value, Value]);
        },
        completion: "COUNT("
    },

    counta: {
        desc: "Returns a count of the number of values in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.COUNTA using apply
            return formulajs.COUNTA.apply(null, values as [Value]);
        },
        completion: "COUNTA("
    },

    countblank: {
        desc: "Returns the number of empty cells in a given range. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.COUNTBLANK using apply
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
            // Call formulajs.COUNTIF using apply
            return formulajs.COUNTIF.apply(null, values as [Value, Value]);
        },
        completion: "COUNTIF("
    },

    countifs: {
        desc: "Returns the count of a range depending on multiple criteria. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.COUNTIFS using apply
            return formulajs.COUNTIFS.apply(null, values as [Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value]);
        },
        completion: "COUNTIFS("
    },


    devsq: {
        desc: "Calculates the sum of squares of deviations based on a sample. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("DEVSQ requires exactly 1 arguments.");
            }
            // Call formulajs.DEVSQ using apply
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
            // Call formulajs.EXPONDIST using apply
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
            // Call formulajs.FDIST using apply
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
            // Call formulajs.FINV using apply
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
            // Call formulajs.FISHER using apply
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
            // Call formulajs.FISHERINV using apply
            return formulajs.FISHERINV.apply(null, values as [Value]);
        },
        completion: "FISHERINV("
    },

    forecast: {
        desc: "Calculates the expected y-value for a specified x based on a linear regression of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.FORECAST using apply
            return formulajs.FORECAST.apply(null, values as [Value, Value,Value]);
        },
        completion: "FORECAST("
    },

    frequency: {
        desc: "Calculates the frequency distribution of a one-column array into specified classes. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.FREQUENCY using apply
            return formulajs.FREQUENCY.apply(null, values as [Value, Value]);
        },
        completion: "FREQUENCY("
    },

    gamma: {
        desc: "Returns the Gamma function evaluated at the specified value. .",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("GAMMA requires exactly 1 arguments.");
            }
            // Call formulajs.GAMMA using apply
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
            // Call formulajs.GAMMALN using apply
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
            // Call formulajs.GAUSS using apply
            return formulajs.GAUSS.apply(null, values as [Value]);
        },
        completion: "GAUSS("
    },

    geomean: {
        desc: "Calculates the geometric mean of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.GEOMEAN using apply
            return formulajs.GEOMEAN.apply(null, values as [Value, Value, Value]);
        },
        completion: "GEOMEAN("
    },

    growth: {
        desc: "Given partial data about an exponential growth trend, fits an ideal exponential growth trend and/or predicts further values. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.GROWTH using apply
            return formulajs.GROWTH.apply(null, values as [Value, Value, Value,Value]);
        },
        completion: "GROWTH("
    },

    harmean: {
        desc: "Calculates the harmonic mean of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.HARMEAN using apply
            return formulajs.HARMEAN.apply(null, values as [Value, Value, Value]);
        },
        completion: "HARMEAN("
    },

    hypgeomdist: {
        desc: "Calculates the probability of drawing a certain number of successes in a certain number of tries given a population of a certain size containing a certain number of successes, without replacement of draws. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.HYPGEOMDIST using apply
            return formulajs.HYPGEOMDIST.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "HYPGEOMDIST("
    },

    intercept: {
        desc: "Calculates the y-value at which the line resulting from linear regression of a dataset will intersect the y-axis (x=0). ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.INTERCEPT using apply
            return formulajs.INTERCEPT.apply(null, values as [Value, Value]);
        },
        completion: "INTERCEPT("
    },

    kurt: {
        desc: "Calculates the kurtosis of a dataset, which describes the shape, and in particular the \"peakedness\" of that dataset. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("KURT requires exactly 1 arguments.");
            }
            // Call formulajs.KURT using apply
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
            // Call formulajs.LARGE using apply
            return formulajs.LARGE.apply(null, values as [Value, Value]);
        },
        completion: "LARGE("
    },

    linest: {
        desc: "Given partial data about a linear trend, calculates various parameters about the ideal linear trend using the least-squares method. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.LINEST using apply
            return formulajs.LINEST.apply(null, values as [Value, Value]);
        },
        completion: "LINEST("
    },

    lognormdist: {
        desc: "Returns the value of the log-normal cumulative distribution with given mean and standard deviation at a specified value. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("LOGNORMDIST requires exactly 4 arguments.");
            }
            // Call formulajs.LOGNORMDIST using apply
            return formulajs.LOGNORMDIST.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "LOGNORMDIST("
    },

    max: {
        desc: "Returns the maximum value in a numeric dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.MAX using apply
            return formulajs.MAX.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "MAX("
    },

    maxa: {
        desc: "Returns the maximum numeric value in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.MAXA using apply
            return formulajs.MAXA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "MAXA("
    },

    median: {
        desc: "Returns the median value in a numeric dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.MEDIAN using apply
            return formulajs.MEDIAN.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "MEDIAN("
    },

    min: {
        desc: "Returns the minimum value in a numeric dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.MIN using apply
            return formulajs.MIN.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "MIN("
    },

    mina: {
        desc: "Returns the minimum numeric value in a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.MINA using apply
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
            // Call formulajs.NORMDIST using apply
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
            // Call formulajs.NORMINV using apply
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
            // Call formulajs.NORMSDIST using apply
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
            // Call formulajs.NORMSINV using apply
            return formulajs.NORMSINV.apply(null, values as [Value]);
        },
        completion: "NORMSINV("
    },

    pearson: {
        desc: "Calculates r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.PEARSON using apply
            return formulajs.PEARSON.apply(null, values as [Value, Value]);
        },
        completion: "PEARSON("
    },

    permut: {
        desc: "Returns the number of ways to choose some number of objects from a pool of a given size of objects, considering order. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("PERMUT requires exactly 2 arguments.");
            }
            // Call formulajs.PERMUT using apply
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
            // Call formulajs.PERMUTATIONA using apply
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
            // Call formulajs.PHI using apply
            return formulajs.PHI.apply(null, values as [Value]);
        },
        completion: "PHI("
    },

    prob: {
        desc: "Given a set of values and corresponding probabilities, calculates the probability that a value chosen at random falls between two limits. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.PROB using apply
            return formulajs.PROB.apply(null, values as [Value, Value, Value, Value]);
        },
        completion: "PROB("
    },

    rsq: {
        desc: "Calculates the square of r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.RSQ using apply
            return formulajs.RSQ.apply(null, values as [Value, Value]);
        },
        completion: "RSQ("
    },

    skew: {
        desc: "Calculates the skewness of a dataset, which describes the symmetry of that dataset about the mean. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("SKEW requires exactly 1 arguments.");
            }
            // Call formulajs.SKEW using apply
            return formulajs.SKEW.apply(null, values as [Value]);
        },
        completion: "SKEW("
    },

    slope: {
        desc: "Calculates the slope of the line resulting from linear regression of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.SLOPE using apply
            return formulajs.SLOPE.apply(null, values as [Value, Value]);
        },
        completion: "SLOPE("
    },

    small: {
        desc: "Returns the nth smallest element from a data set, where n is user-defined. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("SMALL requires exactly 2 arguments.");
            }
            // Call formulajs.SMALL using apply
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
            // Call formulajs.STANDARDIZE using apply
            return formulajs.STANDARDIZE.apply(null, values as [Value, Value, Value]);
        },
        completion: "STANDARDIZE("
    },

    stdeva: {
        desc: "Calculates the standard deviation based on a sample, setting text to the value `0`. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.STDEVA using apply
            return formulajs.STDEVA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "STDEVA("
    },

    stdevp: {
        desc: "Calculates the standard deviation based on an entire population. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.STDEVP using apply
            return formulajs.STDEVP.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "STDEVP("
    },

    stdevpa: {
        desc: "Calculates the standard deviation based on an entire population, setting text to the value `0`. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.STDEVPA using apply
            return formulajs.STDEVPA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "STDEVPA("
    },

    steyx: {
        desc: "Calculates the standard error of the predicted y-value for each x in the regression of a dataset. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.STEYX using apply
            return formulajs.STEYX.apply(null, values as [Value,Value]);
        },
        completion: "STEYX("
    },

    tdist: {
        desc: "Calculates the probability for Student\'s t-distribution with a given input (x). ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 3) {
                throw new Error("TDIST requires exactly 3 arguments.");
            }
            // Call formulajs.TDIST using apply
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
            // Call formulajs.TINV using apply
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
            // Call formulajs.TRIMMEAN using apply
            return formulajs.TRIMMEAN.apply(null, values as [Value, Value]);
        },
        completion: "TRIMMEAN("
    },

    vara: {
        desc: "Calculates an estimate of variance based on a sample, setting text to the value `0`. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.VARA using apply
            return formulajs.VARA.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "VARA("
    },

    varp: {
        desc: "Calculates the variance based on an entire population. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.VARP using apply
            return formulajs.VARP.apply(null, values as [Value, Value, Value, Value, Value]);
        },
        completion: "VARP("
    },

    varpa: {
        desc: "Calculates the variance based on an entire population, setting text to the value `0`. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.VARPA using apply
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
            // Call formulajs.ZTEST using apply
            return formulajs.ZTEST.apply(null, values as [Value, Value,Value]);
        },
        completion: "ZTEST("
    },

    char: {
        desc: "Convert a number into a character according to the current Unicode table. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 1) {
                throw new Error("CHAR requires exactly 1 arguments.");
            }
            // Call formulajs.CHAR using apply
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
            // Call formulajs.CLEAN using apply
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
            // Call formulajs.CODE using apply
            return formulajs.CODE.apply(null, values as [Value]);
        },
        completion: "CODE("
    },

    concatenate: {
        desc: "Appends strings to one another. ",
        compute: (values: Array<Value>) : Value => {
            // if (values.length !== 3) {
            //     throw new Error("CONCATENATE requires exactly 3 arguments.");
            // }
            // Call formulajs.CONCATENATE using apply
            return formulajs.CONCATENATE.apply(null, values);
        },
        completion: "CONCATENATE("
    },

    exact: {
        desc: "Tests whether two strings are identical. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 2) {
                throw new Error("EXACT requires exactly 2 arguments.");
            }
            // Call formulajs.EXACT using apply
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
            // Call formulajs.FIND using apply
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
            // Call formulajs.LEFT using apply
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
            // Call formulajs.LEN using apply
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
            // Call formulajs.LOWER using apply
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
            // Call formulajs.MID using apply
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
            // Call formulajs.PROPER using apply
            return formulajs.PROPER.apply(null, values as [Value]);
        },
        completion: "PROPER("
    },



    replace: {
        desc: "Replaces part of a text string with a different text string. ",
        compute: (values: Array<Value>) : Value => {
            if (values.length !== 4) {
                throw new Error("REPLACE requires exactly 4 arguments.");
            }
            // Call formulajs.REPLACE using apply
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
            // Call formulajs.REPT using apply
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
            // Call formulajs.RIGHT using apply
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
            // Call formulajs.ROMAN using apply
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
            // Call formulajs.SEARCH using apply
            return formulajs.SEARCH.apply(null, values as [Value, Value,Value]);
        },
        completion: "SEARCH("
    },

    substitute: {
        desc: "Replaces existing text with new text in a string. ",
        compute: (values: Array<Value>) : Value => {
            // Call formulajs.SUBSTITUTE using apply
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
            // Call formulajs.T using apply
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
            // Call formulajs.TRIM using apply
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
            // Call formulajs.TEXTJOIN using apply
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
            // Call formulajs.UNICHAR using apply
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
            // Call formulajs.UNICODE using apply
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
            // Call formulajs.UPPER using apply
            return formulajs.UPPER.apply(null, values as [Value]);
        },
        completion: "UPPER("
    },
};
