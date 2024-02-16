import * as ohm from "ohm-js"
import fs from "fs"

const grammar = ohm.grammar(fs.readFileSync("./grammars/lang.ohm"))

export function matches(input) {
  const match = grammar.match(input)
  return match.succeeded()
}

