const regexes = {
  canadianPostalCode: /^[A-CDEGHJ-NPR-TV-Z]\d[A-CDEGHJ-NPR-TV-Z] \d[A-CDEGHJ-NPR-TV-Z]\d$/,
  visa: /^4(\d{15}|\d{12})$/,
  masterCard: /^(5[1-5]\d{2}|222[1-9]|22[3-9]\d|2[3-6]\d{2}|27[01]\d|2720)\d{12}$/,
  notThreeEndingInOO:  /^(?!.o{2}$)\p{L}*$/iu,
  divisibleBy16:  /^(((?!0{4}$)[01])*0{4}|0+)$/,
  eightThroughThirtyTwo:  /^([89]|[12]\d|3[0-2])$/,
  notPythonPycharmPyc:  /^(?!py(thon|c?(harm)?)$).*$/u,
  restrictedFloats:  /^\d+(.\d+)?e[+-]?\d{1,3}$/i,
  palindromes2358: /^(?:([abc]?)([abc]?)([abc])[abc]\3\2\1|([abc]?)([abc]?)([abc]?)([abc])\7\6\5\4)$/,
  pythonStringLiterals: /^(?:(?:br|Br|bR|BR|rb|rB|Rb|RB|b|B|r|R|u|U|f|F|fr|Fr|fR|FR|rf|rF|Rf|RF)?(?:'''(?:\\.|[^\\']|'{1,2}(?!'))*'''|"""(?:\\.|[^\\"]|"{1,2}(?!"))*"""|'(?:\\.|[^\\\n'])*'|"(?:\\.|[^\\\n"])*"))$/,
}

export function matches(name, string) {
  return regexes[name].test(string)
}  

