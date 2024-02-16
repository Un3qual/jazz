import * as ohm from "ohm-js"
import fs from "fs"

const g = (name) => ohm.grammar(fs.readFileSync(`./grammars/${name}.ohm`))

const rules = {
  canadianPostalCode: g("postal"),
  visa: g("visa"),
  masterCard: g("mastercard"),
  notThreeEndingInOO: g("notThree"),
  divisibleBy16: g("divisibleBy16"),
  eightThroughThirtyTwo: g("eightThroughThirtyTwo"),
  notPythonPycharmPyc: g("notPythonPycharmPyc"),
  restrictedFloats: g("restrictedFloats"),
  palindromes2358: g("palindromes2358"),
  pyString: g("pyString"),
}

export function matches(ruleName, input) {
  const match = rules[ruleName].match(input)
  return match.succeeded()
}