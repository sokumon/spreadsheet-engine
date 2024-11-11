import * as formulajs from '@formulajs/formulajs'
export const formulas = {

    date: {
        desc: "Converts a provided year, month, and day into a date. ",
        compute(argsArray){
            return formulajs.DATE.apply(null,argsArray);
        },
        completion: "DATE("
    },

    datevalue: {
        desc: "Converts a provided date string in a known format to a date value. ",
        compute(argsArray){
            return formulajs.DATEVALUE.apply(null,argsArray);
        },
        completion: "DATEVALUE("
    },

    day: {
        desc: "Returns the day of the month that a specific date falls on, in numeric format. ",
        compute(argsArray){
            return formulajs.DAY.apply(null,argsArray);
        },
        completion: "DAY("
    },

    days: {
        desc: "Returns the number of days between two dates. .",
        compute(argsArray){
            return formulajs.DAYS.apply(null,argsArray);
        },
        completion: "DAYS("
    },

    days360: {
        desc: "Returns the difference between two days based on the 360 day year used in some financial interest calculations. ",
        compute(argsArray){
            return formulajs.DAYS360.apply(null,argsArray);
        },
        completion: "DAYS360("
    },

    edate: {
        desc: "Returns a date a specified number of months before or after another date. ",
        compute(argsArray){
            return formulajs.EDATE.apply(null,argsArray);
        },
        completion: "EDATE("
    },

    eomonth: {
        desc: "Returns a date representing the last day of a month which falls a specified number of months before or after another date. ",
        compute(argsArray){
            return formulajs.EOMONTH.apply(null,argsArray);
        },
        completion: "EOMONTH("
    },

    hour: {
        desc: "Returns the hour component of a specific time, in numeric format. ",
        compute(argsArray){
            return formulajs.HOUR.apply(null,argsArray);
        },
        completion: "HOUR("
    },

    minute: {
        desc: "Returns the minute component of a specific time, in numeric format. ",
        compute(argsArray){
            return formulajs.MINUTE.apply(null,argsArray);
        },
        completion: "MINUTE("
    },

    isoweeknum: {
        desc: "Returns the number of the ISO week of the year where the provided date falls. ",
        compute(argsArray){
            return formulajs.ISOWEEKNUM.apply(null,argsArray);
        },
        completion: "ISOWEEKNUM("
    },

    month: {
        desc: "Returns the month of the year a specific date falls in, in numeric format. ",
        compute(argsArray){
            return formulajs.MONTH.apply(null,argsArray);
        },
        completion: "MONTH("
    },

    networkdays: {
        desc: "Returns the number of net working days between two provided days. ",
        compute(argsArray){
            return formulajs.NETWORKDAYS.apply(null,argsArray);
        },
        completion: "NETWORKDAYS("
    },

    now: {
        desc: "Returns the current date and time as a date value. ",
        compute(argsArray){
            return formulajs.NOW.apply(null,argsArray);
        },
        completion: "NOW("
    },

    second: {
        desc: "Returns the second component of a specific time, in numeric format. ",
        compute(argsArray){
            return formulajs.SECOND.apply(null,argsArray);
        },
        completion: "SECOND("
    },

    time: {
        desc: "Converts a provided hour, minute, and second into a time. ",
        compute(argsArray){
            return formulajs.TIME.apply(null,argsArray);
        },
        completion: "TIME("
    },

    timevalue: {
        desc: "Returns the fraction of a 24-hour day the time represents. ",
        compute(argsArray){

            return formulajs.TIMEVALUE.apply(null,argsArray);
        },
        completion: "TIMEVALUE("
    },

    today: {
        desc: "Returns the current date as a date value. ",
        compute(argsArray){
            return formulajs.TODAY.apply(null,argsArray);
        },
        completion: "TODAY("
    },

    weekday: {
        desc: "Returns a number representing the day of the week of the date provided. ",
        compute(argsArray){
            return formulajs.WEEKDAY.apply(null,argsArray);
        },
        completion: "WEEKDAY("
    },

    year: {
        desc: "Returns the year specified by a given date. ",
        compute(argsArray){
            return formulajs.YEAR.apply(null,argsArray);
        },
        completion: "YEAR("
    },

    weeknum: {
        desc: "Returns a number representing the week of the year where the provided date falls. ",
        compute(argsArray){
            return formulajs.WEEKNUM.apply(null,argsArray);
        },
        completion: "WEEKNUM("
    },

    workday: {
        desc: "Calculates the end date after a specified number of working days. ",
        compute(argsArray){
            return formulajs.WORKDAY.apply(null,argsArray);
        },
        completion: "WORKDAY("
    },

    yearfrac: {
        desc: "Returns the number of years, including fractional years, between two dates using a specified day count convention. ",
        compute(argsArray){
            return formulajs.YEARFRAC.apply(null,argsArray);
        },
        completion: "YEARFRAC("
    },

    accrint: {
        desc: "Calculates the accrued interest of a security that has periodic payments. ",
        compute(argsArray){
            return formulajs.ACCRINT.apply(null,argsArray);
        },
        completion: "ACCRINT("
    },

    cumipmt: {
        desc: "Calculates the cumulative interest over a range of payment periods for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute(argsArray){
            return formulajs.CUMIPMT.apply(null,argsArray);
        },
        completion: "CUMIPMT("
    },

    cumprinc: {
        desc: "Calculates the cumulative principal paid over a range of payment periods for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute(argsArray){
            return formulajs.CUMPRINC.apply(null,argsArray);
        },
        completion: "CUMPRINC("
    },

    db: {
        desc: "Calculates the depreciation of an asset for a specified period using the arithmetic declining balance method. ",
        compute(argsArray){
            return formulajs.DB.apply(null,argsArray);
        },
        completion: "DB("
    },

    ddb: {
        desc: "Calculates the depreciation of an asset for a specified period using the double-declining balance method. ",
        compute(argsArray){
            return formulajs.DDB.apply(null,argsArray);
        },
        completion: "DDB("
    },

    dollarde: {
        desc: "Converts a price quotation given as a decimal fraction into a decimal value. ",
        compute(argsArray){
            return formulajs.DOLLARDE.apply(null,argsArray);
        },
        completion: "DOLLARDE("
    },

    dollarfr: {
        desc: "Converts a price quotation given as a decimal value into a decimal fraction. ",
        compute(argsArray){
            return formulajs.DOLLARFR.apply(null,argsArray);
        },
        completion: "DOLLARFR("
    },

    effect: {
        desc: "Calculates the annual effective interest rate given the nominal rate and number of compounding periods per year. ",
        compute(argsArray){
            return formulajs.EFFECT.apply(null,argsArray);
        },
        completion: "EFFECT("
    },

    fv: {
        desc: "Calculates the future value of an annuity investment based on constant-amount periodic payments and a constant interest rate. ",
        compute(argsArray){
            return formulajs.FV.apply(null,argsArray);
        },
        completion: "FV("
    },

    fvschedule: {
        desc: "Calculates the future value of some principal based on a specified series of potentially varying interest rates. ",
        compute(argsArray){
            return formulajs.FVSCHEDULE.apply(null,argsArray);
        },
        completion: "FVSCHEDULE("
    },

    ipmt: {
        desc: "Calculates the payment on interest for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute(argsArray){
            return formulajs.IPMT.apply(null,argsArray);
        },
        completion: "IPMT("
    },

    irr: {
        desc: "Calculates the internal rate of return on an investment based on a series of periodic cash flows. ",
        compute(argsArray){
            return formulajs.IRR.apply(null,argsArray);
        },
        completion: "IRR("
    },

    ispmt: {
        desc: "The ISPMT function calculates the interest paid during a particular period of an investment. .",
        compute(argsArray){
            return formulajs.ISPMT.apply(null,argsArray);
        },
        completion: "ISPMT("
    },

    mirr: {
        desc: "Calculates the modified internal rate of return on an investment based on a series of periodic cash flows and the difference between the interest rate paid on financing versus the return received on reinvested income. ",
        compute(argsArray){
            return formulajs.MIRR.apply(null,argsArray);
        },
        completion: "MIRR("
    },

    nominal: {
        desc: "Calculates the annual nominal interest rate given the effective rate and number of compounding periods per year. ",
        compute(argsArray){
            return formulajs.NOMINAL.apply(null,argsArray);
        },
        completion: "NOMINAL("
    },

    nper: {
        desc: "Calculates the number of payment periods for an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute(argsArray){
            return formulajs.NPER.apply(null,argsArray);
        },
        completion: "NPER("
    },

    npv: {
        desc: "Calculates the net present value of an investment based on a series of periodic cash flows and a discount rate. ",
        compute(argsArray){
            return formulajs.NPV.apply(null,argsArray);
        },
        completion: "NPV("
    },

    pduration: {
        desc: "Returns the number of periods for an investment to reach a specific value at a given rate. .",
        compute(argsArray){
            return formulajs.PDURATION.apply(null,argsArray);
        },
        completion: "PDURATION("
    },

    pmt: {
        desc: "Calculates the periodic payment for an annuity investment based on constant-amount periodic payments and a constant interest rate. ",
        compute(argsArray){
            return formulajs.PMT.apply(null,argsArray);
        },
        completion: "PMT("
    },

    ppmt: {
        desc: "Calculates the payment on the principal of an investment based on constant-amount periodic payments and a constant interest rate. ",
        compute(argsArray){
            return formulajs.PPMT.apply(null,argsArray);
        },
        completion: "PPMT("
    },

    pv: {
        desc: "Calculates the present value of an annuity investment based on constant-amount periodic payments and a constant interest rate. ",
        compute(argsArray){
            return formulajs.PV.apply(null,argsArray);
        },
        completion: "PV("
    },

    rate: {
        desc: "Calculates the interest rate of an annuity investment based on constant-amount periodic payments and the assumption of a constant interest rate. ",
        compute(argsArray){
            return formulajs.RATE.apply(null,argsArray);
        },
        completion: "RATE("
    },

    bin2dec: {
        desc: "Converts a signed binary number to decimal format. ",
        compute(argsArray){
            return formulajs.BIN2DEC.apply(null,argsArray);
        },
        completion: "BIN2DEC("
    },

    bin2hex: {
        desc: "Converts a signed binary number to signed hexadecimal format. ",
        compute(argsArray){
            console.log(Array.isArray(argsArray))
            return formulajs.BIN2HEX.apply(null,argsArray);
        },
        completion: "BIN2HEX("
    },

    bin2oct: {
        desc: "Converts a signed binary number to signed octal format. ",
        compute(argsArray){
            return formulajs.BIN2OCT.apply(null,argsArray);
        },
        completion: "BIN2OCT("
    },

    bitand: {
        desc: "Bitwise boolean AND of two numbers. .",
        compute(argsArray){
            return formulajs.BITAND.apply(null,argsArray);
        },
        completion: "BITAND("
    },

    bitlshift: {
        desc: "Shifts the bits of the input a certain number of places to the left. .",
        compute(argsArray){
            return formulajs.BITLSHIFT.apply(null,argsArray);
        },
        completion: "BITLSHIFT("
    },

    bitor: {
        desc: "Bitwise boolean OR of 2 numbers. .",
        compute(argsArray){
            return formulajs.BITOR.apply(null,argsArray);
        },
        completion: "BITOR("
    },

    bitrshift: {
        desc: "Shifts the bits of the input a certain number of places to the right. .",
        compute(argsArray){
            return formulajs.BITRSHIFT.apply(null,argsArray);
        },
        completion: "BITRSHIFT("
    },

    bitxor: {
        desc: "Bitwise XOR (exclusive OR) of 2 numbers. .",
        compute(argsArray){
            return formulajs.BITXOR.apply(null,argsArray);
        },
        completion: "BITXOR("
    },

    complex: {
        desc: "Creates a complex number given real and imaginary coefficients. ",
        compute(argsArray){
            return formulajs.COMPLEX.apply(null,argsArray);
        },
        completion: "COMPLEX("
    },

    convert: {
        desc: "Converts a numeric value to a different unit of measure. ",
        compute(argsArray){
            return formulajs.CONVERT.apply(null,argsArray);
        },
        completion: "CONVERT("
    },

    dec2bin: {
        desc: "Converts a decimal number to signed binary format. ",
        compute(argsArray){
            return formulajs.DEC2BIN.apply(null,argsArray);
        },
        completion: "DEC2BIN("
    },

    dec2hex: {
        desc: "Converts a decimal number to signed hexadecimal format. ",
        compute(argsArray){
            return formulajs.DEC2HEX.apply(null,argsArray);
        },
        completion: "DEC2HEX("
    },

    dec2oct: {
        desc: "Converts a decimal number to signed octal format. ",
        compute(argsArray){
            return formulajs.DEC2OCT.apply(null,argsArray);
        },
        completion: "DEC2OCT("
    },

    delta: {
        desc: "Compare two numeric values, returning 1 if they\'re equal. ",
        compute(argsArray){
            return formulajs.DELTA.apply(null,argsArray);
        },
        completion: "DELTA("
    },

    erf: {
        desc: "The ERF function returns the integral of the Gauss error function over an interval of values. .",
        compute(argsArray){
            return formulajs.ERF.apply(null,argsArray);
        },
        completion: "ERF("
    },

    erfc: {
        desc: "Returns the complementary Gauss error function of a value. ",
        compute(argsArray){
            return formulajs.ERFC.apply(null,argsArray);
        },
        completion: "ERFC("
    },

    gestep: {
        desc: "Returns 1 if the rate is strictly greater than or equal to the provided step value or 0 otherwise. If no step value is provided then the default value of 0 will be used. .",
        compute(argsArray){
            return formulajs.GESTEP.apply(null,argsArray);
        },
        completion: "GESTEP("
    },

    hex2bin: {
        desc: "Converts a signed hexadecimal number to signed binary format. ",
        compute(argsArray){
            return formulajs.HEX2BIN.apply(null,argsArray);
        },
        completion: "HEX2BIN("
    },

    hex2dec: {
        desc: "Converts a signed hexadecimal number to decimal format. ",
        compute(argsArray){
            return formulajs.HEX2DEC.apply(null,argsArray);
        },
        completion: "HEX2DEC("
    },

    hex2oct: {
        desc: "Converts a signed hexadecimal number to signed octal format. ",
        compute(argsArray){
            return formulajs.HEX2OCT.apply(null,argsArray);
        },
        completion: "HEX2OCT("
    },

    imabs: {
        desc: "Returns absolute value of a complex number. ",
        compute(argsArray){
            return formulajs.IMABS.apply(null,argsArray);
        },
        completion: "IMABS("
    },

    imaginary: {
        desc: "Returns the imaginary coefficient of a complex number. ",
        compute(argsArray){
            return formulajs.IMAGINARY.apply(null,argsArray);
        },
        completion: "IMAGINARY("
    },

    imargument: {
        desc: "The IMARGUMENT function returns the angle (also known as the argument or \theta) of the given complex number in radians. .",
        compute(argsArray){
            return formulajs.IMARGUMENT.apply(null,argsArray);
        },
        completion: "IMARGUMENT("
    },

    imconjugate: {
        desc: "Returns the complex conjugate of a number. ",
        compute(argsArray){
            return formulajs.IMCONJUGATE.apply(null,argsArray);
        },
        completion: "IMCONJUGATE("
    },

    imcos: {
        desc: "The IMCOS function returns the cosine of the given complex number. .",
        compute(argsArray){
            return formulajs.IMCOS.apply(null,argsArray);
        },
        completion: "IMCOS("
    },

    imcosh: {
        desc: "Returns the hyperbolic cosine of the given complex number. For example, a given complex number \"x+yi\" returns \"cosh(x+yi).\" .",
        compute(argsArray){
            return formulajs.IMCOSH.apply(null,argsArray);
        },
        completion: "IMCOSH("
    },

    imcot: {
        desc: "Returns the cotangent of the given complex number. For example, a given complex number \"x+yi\" returns \"cot(x+yi).\" .",
        compute(argsArray){
            return formulajs.IMCOT.apply(null,argsArray);
        },
        completion: "IMCOT("
    },

    imcsc: {
        desc: "Returns the cosecant of the given complex number. .",
        compute(argsArray){
            return formulajs.IMCSC.apply(null,argsArray);
        },
        completion: "IMCSC("
    },

    imcsch: {
        desc: "Returns the hyperbolic cosecant of the given complex number. For example, a given complex number \"x+yi\" returns \"csch(x+yi).\" .",
        compute(argsArray){
            return formulajs.IMCSCH.apply(null,argsArray);
        },
        completion: "IMCSCH("
    },

    imdiv: {
        desc: "Returns one complex number divided by another. ",
        compute(argsArray){
            return formulajs.IMDIV.apply(null,argsArray);
        },
        completion: "IMDIV("
    },

    imexp: {
        desc: "Returns Euler\'s number, e (~2.718) raised to a complex power. .",
        compute(argsArray){
            return formulajs.IMEXP.apply(null,argsArray);
        },
        completion: "IMEXP("
    },

    imln: {
        desc: "Returns the logarithm of a complex number, base e (Euler\'s number). ",
        compute(argsArray){
            return formulajs.IMLN.apply(null,argsArray);
        },
        completion: "IMLN("
    },

    imlog10: {
        desc: "Returns the logarithm of a complex number with base 10. .",
        compute(argsArray){
            return formulajs.IMLOG10.apply(null,argsArray);
        },
        completion: "IMLOG10("
    },

    imlog2: {
        desc: "Returns the logarithm of a complex number with base 2. .",
        compute(argsArray){
            return formulajs.IMLOG2.apply(null,argsArray);
        },
        completion: "IMLOG2("
    },

    impower: {
        desc: "Returns a complex number raised to a power. ",
        compute(argsArray){
            return formulajs.IMPOWER.apply(null,argsArray);
        },
        completion: "IMPOWER("
    },

    improduct: {
        desc: "Returns the result of multiplying a series of complex numbers together. ",
        compute(argsArray){
            return formulajs.IMPRODUCT.apply(null,argsArray);
        },
        completion: "IMPRODUCT("
    },

    imreal: {
        desc: "Returns the real coefficient of a complex number. ",
        compute(argsArray){
            return formulajs.IMREAL.apply(null,argsArray);
        },
        completion: "IMREAL("
    },

    imsec: {
        desc: "Returns the secant of the given complex number. For example, a given complex number \"x+yi\" returns \"sec(x+yi).\" .",
        compute(argsArray){
            return formulajs.IMSEC.apply(null,argsArray);
        },
        completion: "IMSEC("
    },

    imsech: {
        desc: "Returns the hyperbolic secant of the given complex number. For example, a given complex number \"x+yi\" returns \"sech(x+yi).\" .",
        compute(argsArray){
            return formulajs.IMSECH.apply(null,argsArray);
        },
        completion: "IMSECH("
    },

    imsin: {
        desc: "Returns the sine of the given complex number. .",
        compute(argsArray){
            return formulajs.IMSIN.apply(null,argsArray);
        },
        completion: "IMSIN("
    },

    imsinh: {
        desc: "Returns the hyperbolic sine of the given complex number. For example, a given complex number \"x+yi\" returns \"sinh(x+yi).\" .",
        compute(argsArray){
            return formulajs.IMSINH.apply(null,argsArray);
        },
        completion: "IMSINH("
    },

    imsqrt: {
        desc: "Computes the square root of a complex number. ",
        compute(argsArray){
            return formulajs.IMSQRT.apply(null,argsArray);
        },
        completion: "IMSQRT("
    },

    imsub: {
        desc: "Returns the difference between two complex numbers. ",
        compute(argsArray){
            return formulajs.IMSUB.apply(null,argsArray);
        },
        completion: "IMSUB("
    },

    imsum: {
        desc: "Returns the sum of a series of complex numbers. ",
        compute(argsArray){
            return formulajs.IMSUM.apply(null,argsArray);
        },
        completion: "IMSUM("
    },

    imtan: {
        desc: "Returns the tangent of the given complex number. .",
        compute(argsArray){
            return formulajs.IMTAN.apply(null,argsArray);
        },
        completion: "IMTAN("
    },

    oct2bin: {
        desc: "Converts a signed octal number to signed binary format. ",
        compute(argsArray){
            return formulajs.OCT2BIN.apply(null,argsArray);
        },
        completion: "OCT2BIN("
    },

    oct2dec: {
        desc: "Converts a signed octal number to decimal format. ",
        compute(argsArray){
            return formulajs.OCT2DEC.apply(null,argsArray);
        },
        completion: "OCT2DEC("
    },

    oct2hex: {
        desc: "Converts a signed octal number to signed hexadecimal format. ",
        compute(argsArray){
            return formulajs.OCT2HEX.apply(null,argsArray);
        },
        completion: "OCT2HEX("
    },

    and: {
        desc: "Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false. ",
        compute(argsArray){
            return formulajs.AND.apply(null,argsArray);
        },
        completion: "AND("
    },

    if: {
        desc: "Returns one value if a logical expression is `TRUE` and another if it is `FALSE`. ",
        compute(argsArray){
            return formulajs.IF.apply(null,argsArray);
        },
        completion: "IF("
    },

    ifs: {
        desc: "Evaluates multiple conditions and returns a value that corresponds to the first true condition. .",
        compute(argsArray){
            return formulajs.IFS.apply(null,argsArray);
        },
        completion: "IFS("
    },

    iferror: {
        desc: "Returns the first argument if it is not an error value, otherwise returns the second argument if present, or a blank if the second argument is absent. ",
        compute(argsArray){
            return formulajs.IFERROR.apply(null,argsArray);
        },
        completion: "IFERROR("
    },

    ifna: {
        desc: "Evaluates a value. If the value is an #N/A error, returns the specified value. .",
        compute(argsArray){
            return formulajs.IFNA.apply(null,argsArray);
        },
        completion: "IFNA("
    },

    not: {
        desc: "Returns the opposite of a logical value - `NOT(TRUE)` returns `FALSE`; `NOT(FALSE)` returns `TRUE`. ",
        compute(argsArray){
            return formulajs.NOT.apply(null,argsArray);
        },
        completion: "NOT("
    },

    or: {
        desc: "Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false. ",
        compute(argsArray){
            return formulajs.OR.apply(null,argsArray);
        },
        completion: "OR("
    },

    switch: {
        desc: "Tests an expression against a list of cases and returns the corresponding value of the first matching case, with an optional default value if nothing else is met. ",
        compute(argsArray){
            return formulajs.SWITCH.apply(null,argsArray);
        },
        completion: "SWITCH("
    },

    xor: {
        desc: "The XOR function performs an exclusive or of 2 numbers that returns a 1 if the numbers are different, and a 0 otherwise. .",
        compute(argsArray){
            return formulajs.XOR.apply(null,argsArray);
        },
        completion: "XOR("
    },

    abs: {
        desc: "Returns the absolute value of a number. ",
        compute(argsArray){
            return formulajs.ABS.apply(null,argsArray);
        },
        completion: "ABS("
    },

    acos: {
        desc: "Returns the inverse cosine of a value, in radians. ",
        compute(argsArray){
            return formulajs.ACOS.apply(null,argsArray);
        },
        completion: "ACOS("
    },

    acosh: {
        desc: "Returns the inverse hyperbolic cosine of a number. ",
        compute(argsArray){
            return formulajs.ACOSH.apply(null,argsArray);
        },
        completion: "ACOSH("
    },

    acot: {
        desc: "Returns the inverse cotangent of a value, in radians. .",
        compute(argsArray){
            return formulajs.ACOT.apply(null,argsArray);
        },
        completion: "ACOT("
    },

    acoth: {
        desc: "Returns the inverse hyperbolic cotangent of a value, in radians. Must not be between -1 and 1, inclusive. .",
        compute(argsArray){
            return formulajs.ACOTH.apply(null,argsArray);
        },
        completion: "ACOTH("
    },

    arabic: {
        desc: "Computes the value of a Roman numeral. ",
        compute(argsArray){
            return formulajs.ARABIC.apply(null,argsArray);
        },
        completion: "ARABIC("
    },

    asin: {
        desc: "Returns the inverse sine of a value, in radians. ",
        compute(argsArray){
            return formulajs.ASIN.apply(null,argsArray);
        },
        completion: "ASIN("
    },

    asinh: {
        desc: "Returns the inverse hyperbolic sine of a number. ",
        compute(argsArray){
            return formulajs.ASINH.apply(null,argsArray);
        },
        completion: "ASINH("
    },

    atan: {
        desc: "Returns the inverse tangent of a value, in radians. ",
        compute(argsArray){
            return formulajs.ATAN.apply(null,argsArray);
        },
        completion: "ATAN("
    },

    atan2: {
        desc: "Returns the angle between the x-axis and a line segment from the origin (0,0) to specified coordinate pair (`x`,`y`), in radians. ",
        compute(argsArray){
            return formulajs.ATAN2.apply(null,argsArray);
        },
        completion: "ATAN2("
    },

    atanh: {
        desc: "Returns the inverse hyperbolic tangent of a number. ",
        compute(argsArray){
            return formulajs.ATANH.apply(null,argsArray);
        },
        completion: "ATANH("
    },

    base: {
        desc: "Converts a number into a text representation in another base, for example, base 2 for binary. .",
        compute(argsArray){
            return formulajs.BASE.apply(null,argsArray);
        },
        completion: "BASE("
    },

    ceiling: {
        desc: "Rounds a number up to the nearest integer multiple of specified significance. ",
        compute(argsArray){
            return formulajs.CEILING.apply(null,argsArray);
        },
        completion: "CEILING("
    },

    combin: {
        desc: "Returns the number of ways to choose some number of objects from a pool of a given size of objects. ",
        compute(argsArray){
            return formulajs.COMBIN.apply(null,argsArray);
        },
        completion: "COMBIN("
    },

    combina: {
        desc: "Returns the number of ways to choose some number of objects from a pool of a given size of objects, including ways that choose the same object multiple times. .",
        compute(argsArray){
            return formulajs.COMBINA.apply(null,argsArray);
        },
        completion: "COMBINA("
    },

    cos: {
        desc: "Returns the cosine of an angle provided in radians. ",
        compute(argsArray){
            return formulajs.COS.apply(null,argsArray);
        },
        completion: "COS("
    },

    cosh: {
        desc: "Returns the hyperbolic cosine of any real number. ",
        compute(argsArray){
            return formulajs.COSH.apply(null,argsArray);
        },
        completion: "COSH("
    },

    cot: {
        desc: "Cotangent of an angle provided in radians. .",
        compute(argsArray){
            return formulajs.COT.apply(null,argsArray);
        },
        completion: "COT("
    },

    coth: {
        desc: "Returns the hyperbolic cotangent of any real number. .",
        compute(argsArray){
            return formulajs.COTH.apply(null,argsArray);
        },
        completion: "COTH("
    },

    csc: {
        desc: "Returns the cosecant of an angle provided in radians. .",
        compute(argsArray){
            return formulajs.CSC.apply(null,argsArray);
        },
        completion: "CSC("
    },

    csch: {
        desc: "The CSCH function returns the hyperbolic cosecant of any real number. .",
        compute(argsArray){
            return formulajs.CSCH.apply(null,argsArray);
        },
        completion: "CSCH("
    },

    decimal: {
        desc: "The DECIMAL function converts the text representation of a number in another base, to base 10 (decimal). .",
        compute(argsArray){
            return formulajs.DECIMAL.apply(null,argsArray);
        },
        completion: "DECIMAL("
    },

    erf: {
        desc: "The ERF function returns the integral of the Gauss error function over an interval of values. .",
        compute(argsArray){
            return formulajs.ERF.apply(null,argsArray);
        },
        completion: "ERF("
    },

    erfc: {
        desc: "Returns the complementary Gauss error function of a value. ",
        compute(argsArray){
            return formulajs.ERFC.apply(null,argsArray);
        },
        completion: "ERFC("
    },

    even: {
        desc: "Rounds a number up to the nearest even integer. ",
        compute(argsArray){
            return formulajs.EVEN.apply(null,argsArray);
        },
        completion: "EVEN("
    },

    exp: {
        desc: "Returns Euler\'s number, e (~2.718) raised to a power. ",
        compute(argsArray){
            return formulajs.EXP.apply(null,argsArray);
        },
        completion: "EXP("
    },

    fact: {
        desc: "Returns the factorial of a number. ",
        compute(argsArray){
            return formulajs.FACT.apply(null,argsArray);
        },
        completion: "FACT("
    },

    factdouble: {
        desc: "Returns the \"double factorial\" of a number. ",
        compute(argsArray){
            return formulajs.FACTDOUBLE.apply(null,argsArray);
        },
        completion: "FACTDOUBLE("
    },

    floor: {
        desc: "Rounds a number down to the nearest integer multiple of specified significance. ",
        compute(argsArray){
            return formulajs.FLOOR.apply(null,argsArray);
        },
        completion: "FLOOR("
    },

    gcd: {
        desc: "Returns the greatest common divisor of one or more integers. ",
        compute(argsArray){
            return formulajs.GCD.apply(null,argsArray);
        },
        completion: "GCD("
    },

    int: {
        desc: "Rounds a number down to the nearest integer that is less than or equal to it. ",
        compute(argsArray){
            return formulajs.INT.apply(null,argsArray);
        },
        completion: "INT("
    },

    iseven: {
        desc: "Checks whether the provided value is even. ",
        compute(argsArray){
            return formulajs.ISEVEN.apply(null,argsArray);
        },
        completion: "ISEVEN("
    },

    isodd: {
        desc: "Checks whether the provided value is odd. ",
        compute(argsArray){
            return formulajs.ISODD.apply(null,argsArray);
        },
        completion: "ISODD("
    },

    lcm: {
        desc: "Returns the least common multiple of one or more integers. ",
        compute(argsArray){
            return formulajs.LCM.apply(null,argsArray);
        },
        completion: "LCM("
    },

    ln: {
        desc: "Returns the the logarithm of a number, base e (Euler\'s number). ",
        compute(argsArray){
            return formulajs.LN.apply(null,argsArray);
        },
        completion: "LN("
    },

    log: {
        desc: "Returns the the logarithm of a number given a base. ",
        compute(argsArray){
            return formulajs.LOG.apply(null,argsArray);
        },
        completion: "LOG("
    },

    log10: {
        desc: "Returns the the logarithm of a number, base 10. ",
        compute(argsArray){
            return formulajs.LOG10.apply(null,argsArray);
        },
        completion: "LOG10("
    },

    mod: {
        desc: "Returns the result of the modulo operator, the remainder after a division operation. ",
        compute(argsArray){
            return formulajs.MOD.apply(null,argsArray);
        },
        completion: "MOD("
    },

    mround: {
        desc: "Rounds one number to the nearest integer multiple of another. ",
        compute(argsArray){
            return formulajs.MROUND.apply(null,argsArray);
        },
        completion: "MROUND("
    },

    multinomial: {
        desc: "Returns the factorial of the sum of values divided by the product of the values\' factorials. ",
        compute(argsArray){
            return formulajs.MULTINOMIAL.apply(null,argsArray);
        },
        completion: "MULTINOMIAL("
    },

    odd: {
        desc: "Rounds a number up to the nearest odd integer. ",
        compute(argsArray){
            return formulajs.ODD.apply(null,argsArray);
        },
        completion: "ODD("
    },

    power: {
        desc: "Returns a number raised to a power. ",
        compute(argsArray){
            return formulajs.POWER.apply(null,argsArray);
        },
        completion: "POWER("
    },

    product: {
        desc: "Returns the result of multiplying a series of numbers together. ",
        compute(argsArray){
            return formulajs.PRODUCT.apply(null,argsArray);
        },
        completion: "PRODUCT("
    },

    quotient: {
        desc: "Returns one number divided by another. ",
        compute(argsArray){
            return formulajs.QUOTIENT.apply(null,argsArray);
        },
        completion: "QUOTIENT("
    },

    radians: {
        desc: "Converts an angle value in degrees to radians. ",
        compute(argsArray){
            return formulajs.RADIANS.apply(null,argsArray);
        },
        completion: "RADIANS("
    },

    rand: {
        desc: "Returns a random number between 0 inclusive and 1 exclusive. ",
        compute(argsArray){
            return formulajs.RAND.apply(null,argsArray);
        },
        completion: "RAND("
    },

    randbetween: {
        desc: "Returns a uniformly random integer between two values, inclusive. ",
        compute(argsArray){
            return formulajs.RANDBETWEEN.apply(null,argsArray);
        },
        completion: "RANDBETWEEN("
    },

    round: {
        desc: "Rounds a number to a certain number of decimal places according to standard rules. ",
        compute(argsArray){
            return formulajs.ROUND.apply(null,argsArray);
        },
        completion: "ROUND("
    },

    rounddown: {
        desc: "Rounds a number to a certain number of decimal places, always rounding down to the next valid increment. ",
        compute(argsArray){
            return formulajs.ROUNDDOWN.apply(null,argsArray);
        },
        completion: "ROUNDDOWN("
    },

    roundup: {
        desc: "Rounds a number to a certain number of decimal places, always rounding up to the next valid increment. ",
        compute(argsArray){
            return formulajs.ROUNDUP.apply(null,argsArray);
        },
        completion: "ROUNDUP("
    },

    sec: {
        desc: "The SEC function returns the secant of an angle, measured in radians. .",
        compute(argsArray){
            return formulajs.SEC.apply(null,argsArray);
        },
        completion: "SEC("
    },

    sech: {
        desc: "The SECH function returns the hyperbolic secant of an angle. ",
        compute(argsArray){
            return formulajs.SECH.apply(null,argsArray);
        },
        completion: "SECH("
    },

    sign: {
        desc: "Given an input number, returns `-1` if it is negative, `1` if positive, and `0` if it is zero. ",
        compute(argsArray){
            return formulajs.SIGN.apply(null,argsArray);
        },
        completion: "SIGN("
    },

    sin: {
        desc: "Returns the sine of an angle provided in radians. ",
        compute(argsArray){
            return formulajs.SIN.apply(null,argsArray);
        },
        completion: "SIN("
    },

    sinh: {
        desc: "Returns the hyperbolic sine of any real number. ",
        compute(argsArray){
            return formulajs.SINH.apply(null,argsArray);
        },
        completion: "SINH("
    },

    sqrt: {
        desc: "Returns the positive square root of a positive number. ",
        compute(argsArray){
            return formulajs.SQRT.apply(null,argsArray);
        },
        completion: "SQRT("
    },

    sqrtpi: {
        desc: "Returns the positive square root of the product of Pi and the given positive number. ",
        compute(argsArray){
            return formulajs.SQRTPI.apply(null,argsArray);
        },
        completion: "SQRTPI("
    },

    subtotal: {
        desc: "Returns a subtotal for a vertical range of cells using a specified aggregation function. ",
        compute(argsArray){
            return formulajs.SUBTOTAL.apply(null,argsArray);
        },
        completion: "SUBTOTAL("
    },

    sum: {
        desc: "Returns the sum of a series of numbers and/or cells. ",
        compute(argsArray){
            return formulajs.SUM.apply(null,argsArray);
        },
        completion: "SUM("
    },

    sumif: {
        desc: "Returns a conditional sum across a range. ",
        compute(argsArray){
            return formulajs.SUMIF.apply(null,argsArray);
        },
        completion: "SUMIF("
    },

    sumifs: {
        desc: "Returns the sum of a range depending on multiple criteria. ",
        compute(argsArray){
            return formulajs.SUMIFS.apply(null,argsArray);
        },
        completion: "SUMIFS("
    },

    sumproduct: {
        desc: "Calculates the sum of the products of corresponding entries in two equal-sized arrays or ranges. ",
        compute(argsArray){
            return formulajs.SUMPRODUCT.apply(null,argsArray);
        },
        completion: "SUMPRODUCT("
    },

    sumsq: {
        desc: "Returns the sum of the squares of a series of numbers and/or cells. ",
        compute(argsArray){
            return formulajs.SUMSQ.apply(null,argsArray);
        },
        completion: "SUMSQ("
    },

    sumx2my2: {
        desc: "Calculates the sum of the differences of the squares of values in two arrays. ",
        compute(argsArray){
            return formulajs.SUMX2MY2.apply(null,argsArray);
        },
        completion: "SUMX2MY2("
    },

    sumx2py2: {
        desc: "Calculates the sum of the sums of the squares of values in two arrays. ",
        compute(argsArray){
            return formulajs.SUMX2PY2.apply(null,argsArray);
        },
        completion: "SUMX2PY2("
    },

    sumxmy2: {
        desc: "Calculates the sum of the squares of differences of values in two arrays. ",
        compute(argsArray){
            return formulajs.SUMXMY2.apply(null,argsArray);
        },
        completion: "SUMXMY2("
    },

    tan: {
        desc: "Returns the tangent of an angle provided in radians. ",
        compute(argsArray){
            return formulajs.TAN.apply(null,argsArray);
        },
        completion: "TAN("
    },

    tanh: {
        desc: "Returns the hyperbolic tangent of any real number. ",
        compute(argsArray){
            return formulajs.TANH.apply(null,argsArray);
        },
        completion: "TANH("
    },

    trunc: {
        desc: "Truncates a number to a certain number of significant digits by omitting less significant digits. ",
        compute(argsArray){
            return formulajs.TRUNC.apply(null,argsArray);
        },
        completion: "TRUNC("
    },

    avedev: {
        desc: "Calculates the average of the magnitudes of deviations of data from a dataset\'s mean. ",
        compute(argsArray){
            return formulajs.AVEDEV.apply(null,argsArray);
        },
        completion: "AVEDEV("
    },

    average: {
        desc: "Returns the numerical average value in a dataset, ignoring text. ",
        compute(argsArray){
            return formulajs.AVERAGE.apply(null,argsArray);
        },
        completion: "AVERAGE("
    },

    averagea: {
        desc: "Returns the numerical average value in a dataset. ",
        compute(argsArray){
            return formulajs.AVERAGEA.apply(null,argsArray);
        },
        completion: "AVERAGEA("
    },

    averageif: {
        desc: "Returns the average of a range depending on criteria. ",
        compute(argsArray){
            return formulajs.AVERAGEIF.apply(null,argsArray);
        },
        completion: "AVERAGEIF("
    },

    averageifs: {
        desc: "Returns the average of a range depending on multiple criteria. ",
        compute(argsArray){
            return formulajs.AVERAGEIFS.apply(null,argsArray);
        },
        completion: "AVERAGEIFS("
    },

    betadist: {
        desc: "See BETA.DIST.",
        compute(argsArray){
            return formulajs.BETADIST.apply(null,argsArray);
        },
        completion: "BETADIST("
    },

    betainv: {
        desc: "See BETA.INV",
        compute(argsArray){
            return formulajs.BETAINV.apply(null,argsArray);
        },
        completion: "BETAINV("
    },

    binomdist: {
        desc: "Calculates the probability of drawing a certain number of successes (or a maximum number of successes) in a certain number of tries given a population of a certain size containing a certain number of successes, with replacement of draws. ",
        compute(argsArray){
            return formulajs.BINOMDIST.apply(null,argsArray);
        },
        completion: "BINOMDIST("
    },

    correl: {
        desc: "Calculates r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute(argsArray){
            return formulajs.CORREL.apply(null,argsArray);
        },
        completion: "CORREL("
    },

    count: {
        desc: "Returns a count of the number of numeric values in a dataset. ",
        compute(argsArray){
            return formulajs.COUNT.apply(null,argsArray);
        },
        completion: "COUNT("
    },

    counta: {
        desc: "Returns a count of the number of values in a dataset. ",
        compute(argsArray){
            return formulajs.COUNTA.apply(null,argsArray);
        },
        completion: "COUNTA("
    },

    countblank: {
        desc: "Returns the number of empty cells in a given range. ",
        compute(argsArray){
            return formulajs.COUNTBLANK.apply(null,argsArray);
        },
        completion: "COUNTBLANK("
    },

    countif: {
        desc: "Returns a conditional count across a range. ",
        compute(argsArray){
            return formulajs.COUNTIF.apply(null,argsArray);
        },
        completion: "COUNTIF("
    },

    countifs: {
        desc: "Returns the count of a range depending on multiple criteria. ",
        compute(argsArray){
            return formulajs.COUNTIFS.apply(null,argsArray);
        },
        completion: "COUNTIFS("
    },

    countunique: {
        desc: "Counts the number of unique values in a list of specified values and ranges. ",
        compute(argsArray){
            return formulajs.COUNTUNIQUE.apply(null,argsArray);
        },
        completion: "COUNTUNIQUE("
    },

    devsq: {
        desc: "Calculates the sum of squares of deviations based on a sample. ",
        compute(argsArray){
            return formulajs.DEVSQ.apply(null,argsArray);
        },
        completion: "DEVSQ("
    },

    expondist: {
        desc: "See EXPON.DIST",
        compute(argsArray){
            return formulajs.EXPONDIST.apply(null,argsArray);
        },
        completion: "EXPONDIST("
    },

    fdist: {
        desc: "See F.DIST.RT.",
        compute(argsArray){
            return formulajs.FDIST.apply(null,argsArray);
        },
        completion: "FDIST("
    },

    finv: {
        desc: "See F.INV.RT",
        compute(argsArray){
            return formulajs.FINV.apply(null,argsArray);
        },
        completion: "FINV("
    },

    fisher: {
        desc: "Returns the Fisher transformation of a specified value. ",
        compute(argsArray){
            return formulajs.FISHER.apply(null,argsArray);
        },
        completion: "FISHER("
    },

    fisherinv: {
        desc: "Returns the inverse Fisher transformation of a specified value. ",
        compute(argsArray){
            return formulajs.FISHERINV.apply(null,argsArray);
        },
        completion: "FISHERINV("
    },

    forecast: {
        desc: "Calculates the expected y-value for a specified x based on a linear regression of a dataset. ",
        compute(argsArray){
            return formulajs.FORECAST.apply(null,argsArray);
        },
        completion: "FORECAST("
    },

    frequency: {
        desc: "Calculates the frequency distribution of a one-column array into specified classes. ",
        compute(argsArray){
            return formulajs.FREQUENCY.apply(null,argsArray);
        },
        completion: "FREQUENCY("
    },

    gamma: {
        desc: "Returns the Gamma function evaluated at the specified value. .",
        compute(argsArray){
            return formulajs.GAMMA.apply(null,argsArray);
        },
        completion: "GAMMA("
    },

    gammaln: {
        desc: "Returns the the logarithm of a specified Gamma function, base e (Euler\'s number). ",
        compute(argsArray){
            return formulajs.GAMMALN.apply(null,argsArray);
        },
        completion: "GAMMALN("
    },

    gauss: {
        desc: "The GAUSS function returns the probability that a random variable, drawn from a normal distribution, will be between the mean and z standard deviations above (or below) the mean. .",
        compute(argsArray){
            return formulajs.GAUSS.apply(null,argsArray);
        },
        completion: "GAUSS("
    },

    geomean: {
        desc: "Calculates the geometric mean of a dataset. ",
        compute(argsArray){
            return formulajs.GEOMEAN.apply(null,argsArray);
        },
        completion: "GEOMEAN("
    },

    growth: {
        desc: "Given partial data about an exponential growth trend, fits an ideal exponential growth trend and/or predicts further values. ",
        compute(argsArray){
            return formulajs.GROWTH.apply(null,argsArray);
        },
        completion: "GROWTH("
    },

    harmean: {
        desc: "Calculates the harmonic mean of a dataset. ",
        compute(argsArray){
            return formulajs.HARMEAN.apply(null,argsArray);
        },
        completion: "HARMEAN("
    },

    hypgeomdist: {
        desc: "Calculates the probability of drawing a certain number of successes in a certain number of tries given a population of a certain size containing a certain number of successes, without replacement of draws. ",
        compute(argsArray){
            return formulajs.HYPGEOMDIST.apply(null,argsArray);
        },
        completion: "HYPGEOMDIST("
    },

    intercept: {
        desc: "Calculates the y-value at which the line resulting from linear regression of a dataset will intersect the y-axis (x=0). ",
        compute(argsArray){
            return formulajs.INTERCEPT.apply(null,argsArray);
        },
        completion: "INTERCEPT("
    },

    kurt: {
        desc: "Calculates the kurtosis of a dataset, which describes the shape, and in particular the \"peakedness\" of that dataset. ",
        compute(argsArray){
            return formulajs.KURT.apply(null,argsArray);
        },
        completion: "KURT("
    },

    large: {
        desc: "Returns the nth largest element from a data set, where n is user-defined. ",
        compute(argsArray){
            return formulajs.LARGE.apply(null,argsArray);
        },
        completion: "LARGE("
    },

    linest: {
        desc: "Given partial data about a linear trend, calculates various parameters about the ideal linear trend using the least-squares method. ",
        compute(argsArray){
            return formulajs.LINEST.apply(null,argsArray);
        },
        completion: "LINEST("
    },

    lognormdist: {
        desc: "Returns the value of the log-normal cumulative distribution with given mean and standard deviation at a specified value. ",
        compute(argsArray){
            return formulajs.LOGNORMDIST.apply(null,argsArray);
        },
        completion: "LOGNORMDIST("
    },

    max: {
        desc: "Returns the maximum value in a numeric dataset. ",
        compute(argsArray){
            return formulajs.MAX.apply(null,argsArray);
        },
        completion: "MAX("
    },

    maxa: {
        desc: "Returns the maximum numeric value in a dataset. ",
        compute(argsArray){
            return formulajs.MAXA.apply(null,argsArray);
        },
        completion: "MAXA("
    },

    median: {
        desc: "Returns the median value in a numeric dataset. ",
        compute(argsArray){
            return formulajs.MEDIAN.apply(null,argsArray);
        },
        completion: "MEDIAN("
    },

    min: {
        desc: "Returns the minimum value in a numeric dataset. ",
        compute(argsArray){
            return formulajs.MIN.apply(null,argsArray);
        },
        completion: "MIN("
    },

    mina: {
        desc: "Returns the minimum numeric value in a dataset. ",
        compute(argsArray){
            return formulajs.MINA.apply(null,argsArray);
        },
        completion: "MINA("
    },

    normdist: {
        desc: "Returns the value of the normal distribution function (or normal cumulative distribution function) for a specified value, mean, and standard deviation. ",
        compute(argsArray){
            return formulajs.NORMDIST.apply(null,argsArray);
        },
        completion: "NORMDIST("
    },

    norminv: {
        desc: "Returns the value of the inverse normal distribution function for a specified value, mean, and standard deviation. ",
        compute(argsArray){
            return formulajs.NORMINV.apply(null,argsArray);
        },
        completion: "NORMINV("
    },

    normsdist: {
        desc: "Returns the value of the standard normal cumulative distribution function for a specified value. ",
        compute(argsArray){
            return formulajs.NORMSDIST.apply(null,argsArray);
        },
        completion: "NORMSDIST("
    },

    normsinv: {
        desc: "Returns the value of the inverse standard normal distribution function for a specified value. ",
        compute(argsArray){
            return formulajs.NORMSINV.apply(null,argsArray);
        },
        completion: "NORMSINV("
    },

    pearson: {
        desc: "Calculates r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute(argsArray){
            return formulajs.PEARSON.apply(null,argsArray);
        },
        completion: "PEARSON("
    },

    permut: {
        desc: "Returns the number of ways to choose some number of objects from a pool of a given size of objects, considering order. ",
        compute(argsArray){
            return formulajs.PERMUT.apply(null,argsArray);
        },
        completion: "PERMUT("
    },

    permutationa: {
        desc: "Returns the number of permutations for selecting a group of objects (with replacement) from a total number of objects. .",
        compute(argsArray){
            return formulajs.PERMUTATIONA.apply(null,argsArray);
        },
        completion: "PERMUTATIONA("
    },

    phi: {
        desc: "The PHI function returns the value of the normal distribution with mean 0 and standard deviation 1. .",
        compute(argsArray){
            return formulajs.PHI.apply(null,argsArray);
        },
        completion: "PHI("
    },

    prob: {
        desc: "Given a set of values and corresponding probabilities, calculates the probability that a value chosen at random falls between two limits. ",
        compute(argsArray){
            return formulajs.PROB.apply(null,argsArray);
        },
        completion: "PROB("
    },

    rsq: {
        desc: "Calculates the square of r, the Pearson product-moment correlation coefficient of a dataset. ",
        compute(argsArray){
            return formulajs.RSQ.apply(null,argsArray);
        },
        completion: "RSQ("
    },

    skew: {
        desc: "Calculates the skewness of a dataset, which describes the symmetry of that dataset about the mean. ",
        compute(argsArray){
            return formulajs.SKEW.apply(null,argsArray);
        },
        completion: "SKEW("
    },

    slope: {
        desc: "Calculates the slope of the line resulting from linear regression of a dataset. ",
        compute(argsArray){
            return formulajs.SLOPE.apply(null,argsArray);
        },
        completion: "SLOPE("
    },

    small: {
        desc: "Returns the nth smallest element from a data set, where n is user-defined. ",
        compute(argsArray){
            return formulajs.SMALL.apply(null,argsArray);
        },
        completion: "SMALL("
    },

    standardize: {
        desc: "Calculates the normalized equivalent of a random variable given mean and standard deviation of the distribution. ",
        compute(argsArray){
            return formulajs.STANDARDIZE.apply(null,argsArray);
        },
        completion: "STANDARDIZE("
    },

    stdeva: {
        desc: "Calculates the standard deviation based on a sample, setting text to the value `0`. ",
        compute(argsArray){
            return formulajs.STDEVA.apply(null,argsArray);
        },
        completion: "STDEVA("
    },

    stdevp: {
        desc: "Calculates the standard deviation based on an entire population. ",
        compute(argsArray){
            return formulajs.STDEVP.apply(null,argsArray);
        },
        completion: "STDEVP("
    },

    stdevpa: {
        desc: "Calculates the standard deviation based on an entire population, setting text to the value `0`. ",
        compute(argsArray){
            return formulajs.STDEVPA.apply(null,argsArray);
        },
        completion: "STDEVPA("
    },

    steyx: {
        desc: "Calculates the standard error of the predicted y-value for each x in the regression of a dataset. ",
        compute(argsArray){
            return formulajs.STEYX.apply(null,argsArray);
        },
        completion: "STEYX("
    },

    tdist: {
        desc: "Calculates the probability for Student\'s t-distribution with a given input (x). ",
        compute(argsArray){
            return formulajs.TDIST.apply(null,argsArray);
        },
        completion: "TDIST("
    },

    tinv: {
        desc: "See T.INV.2T",
        compute(argsArray){
            return formulajs.TINV.apply(null,argsArray);
        },
        completion: "TINV("
    },

    trimmean: {
        desc: "Calculates the mean of a dataset excluding some proportion of data from the high and low ends of the dataset. ",
        compute(argsArray){
            return formulajs.TRIMMEAN.apply(null,argsArray);
        },
        completion: "TRIMMEAN("
    },

    vara: {
        desc: "Calculates an estimate of variance based on a sample, setting text to the value `0`. ",
        compute(argsArray){
            return formulajs.VARA.apply(null,argsArray);
        },
        completion: "VARA("
    },

    varp: {
        desc: "Calculates the variance based on an entire population. ",
        compute(argsArray){
            return formulajs.VARP.apply(null,argsArray);
        },
        completion: "VARP("
    },

    varpa: {
        desc: "Calculates the variance based on an entire population, setting text to the value `0`. ",
        compute(argsArray){
            return formulajs.VARPA.apply(null,argsArray);
        },
        completion: "VARPA("
    },

    ztest: {
        desc: "See Z.TEST.",
        compute(argsArray){
            return formulajs.ZTEST.apply(null,argsArray);
        },
        completion: "ZTEST("
    },

    char: {
        desc: "Convert a number into a character according to the current Unicode table. ",
        compute(argsArray){
            return formulajs.CHAR.apply(null,argsArray);
        },
        completion: "CHAR("
    },

    clean: {
        desc: "Returns the text with the non-printable ASCII characters removed. ",
        compute(argsArray){
            return formulajs.CLEAN.apply(null,argsArray);
        },
        completion: "CLEAN("
    },

    code: {
        desc: "Returns the numeric Unicode map value of the first character in the string provided. ",
        compute(argsArray){
            return formulajs.CODE.apply(null,argsArray);
        },
        completion: "CODE("
    },

    concatenate: {
        desc: "Appends strings to one another. ",
        compute(argsArray){
            return formulajs.CONCATENATE.apply(null,argsArray);
        },
        completion: "CONCATENATE("
    },

    exact: {
        desc: "Tests whether two strings are identical. ",
        compute(argsArray){
            return formulajs.EXACT.apply(null,argsArray);
        },
        completion: "EXACT("
    },

    find: {
        desc: "Returns the position at which a string is first found within text. ",
        compute(argsArray){
            return formulajs.FIND.apply(null,argsArray);
        },
        completion: "FIND("
    },

    left: {
        desc: "Returns a substring from the beginning of a specified string. ",
        compute(argsArray){
            return formulajs.LEFT.apply(null,argsArray);
        },
        completion: "LEFT("
    },

    len: {
        desc: "Returns the length of a string. ",
        compute(argsArray){
            return formulajs.LEN.apply(null,argsArray);
        },
        completion: "LEN("
    },

    lower: {
        desc: "Converts a specified string to lowercase. ",
        compute(argsArray){
            return formulajs.LOWER.apply(null,argsArray);
        },
        completion: "LOWER("
    },

    mid: {
        desc: "Returns a segment of a string. ",
        compute(argsArray){
            return formulajs.MID.apply(null,argsArray);
        },
        completion: "MID("
    },

    proper: {
        desc: "Capitalizes each word in a specified string. ",
        compute(argsArray){
            return formulajs.PROPER.apply(null,argsArray);
        },
        completion: "PROPER("
    },

    regexextract: {
        desc: "Extracts matching substrings according to a regular expression. ",
        compute(argsArray){
            return formulajs.REGEXEXTRACT.apply(null,argsArray);
        },
        completion: "REGEXEXTRACT("
    },

    regexmatch: {
        desc: "Whether a piece of text matches a regular expression. ",
        compute(argsArray){
            return formulajs.REGEXMATCH.apply(null,argsArray);
        },
        completion: "REGEXMATCH("
    },

    regexreplace: {
        desc: "Replaces part of a text string with a different text string using regular expressions. ",
        compute(argsArray){
            return formulajs.REGEXREPLACE.apply(null,argsArray);
        },
        completion: "REGEXREPLACE("
    },

    replace: {
        desc: "Replaces part of a text string with a different text string. ",
        compute(argsArray){
            return formulajs.REPLACE.apply(null,argsArray);
        },
        completion: "REPLACE("
    },

    rept: {
        desc: "Returns specified text repeated a number of times. ",
        compute(argsArray){
            return formulajs.REPT.apply(null,argsArray);
        },
        completion: "REPT("
    },

    right: {
        desc: "Returns a substring from the end of a specified string. ",
        compute(argsArray){
            return formulajs.RIGHT.apply(null,argsArray);
        },
        completion: "RIGHT("
    },

    roman: {
        desc: "Formats a number in Roman numerals. ",
        compute(argsArray){
            return formulajs.ROMAN.apply(null,argsArray);
        },
        completion: "ROMAN("
    },

    search: {
        desc: "Returns the position at which a string is first found within text. ",
        compute(argsArray){
            return formulajs.SEARCH.apply(null,argsArray);
        },
        completion: "SEARCH("
    },

    split: {
        desc: "Divides text around a specified character or string, and puts each fragment into a separate cell in the row. ",
        compute(argsArray){
            return formulajs.SPLIT.apply(null,argsArray);
        },
        completion: "SPLIT("
    },

    substitute: {
        desc: "Replaces existing text with new text in a string. ",
        compute(argsArray){
            return formulajs.SUBSTITUTE.apply(null,argsArray);
        },
        completion: "SUBSTITUTE("
    },

    t: {
        desc: "Returns string arguments as text. ",
        compute(argsArray){
            return formulajs.T.apply(null,argsArray);
        },
        completion: "T("
    },

    trim: {
        desc: "Removes leading and trailing spaces in a specified string. ",
        compute(argsArray){
            return formulajs.TRIM.apply(null,argsArray);
        },
        completion: "TRIM("
    },

    unichar: {
        desc: "Returns the Unicode character for a number. .",
        compute(argsArray){
            return formulajs.UNICHAR.apply(null,argsArray);
        },
        completion: "UNICHAR("
    },

    unicode: {
        desc: "Returns the decimal Unicode value of the first character of the text. .",
        compute(argsArray){
            return formulajs.UNICODE.apply(null,argsArray);
        },
        completion: "UNICODE("
    },

    upper: {
        desc: "Converts a specified string to uppercase. ",
        compute(argsArray){
            return formulajs.UPPER.apply(null,argsArray);
        },
        completion: "UPPER("
    },
};
