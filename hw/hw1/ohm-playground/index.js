import * as ohm from "ohm-js"
import fs from "fs"

const astroGrammar = ohm.grammar(fs.readFileSync("./astro.ohm"))

const memory = {
  π: { type: "NUM", value: Math.PI, access: "RO" },
  sin: { type: "FUNC", value: Math.sin, paramCount: 1 },
  cos: { type: "FUNC", value: Math.cos, paramCount: 1 },
  sqrt: { type: "FUNC", value: Math.sqrt, paramCount: 1 },
  hypot: { type: "FUNC", value: Math.hypot, paramCount: 2 },
}

function check(condition, message, at) {
  if (!condition) throw new Error(`${at.source.getLineAndColumnMessage()}${message}`)
}

const evaluator = astroGrammar.createSemantics().addOperation("eval", {
  Program(statements) {
    for (const statement of statements.children) statement.eval()
  },
  Statement_assignment(id, _eq, expression, _semicolon) {
    const entity = memory[id.sourceString]
    check(!entity || entity?.type === "NUM", "Cannot assign", id)
    check(!entity || entity?.access === "RW", `${id.sourceString} not writable`, id)
    memory[id.sourceString] = { type: "NUM", value: expression.eval(), access: "RW" }
  },
  Statement_print(_print, expression, _semicolon) {
    console.log(expression)
    console.log(expression.eval())
  },
  Statement_if(_if, _leftParen, condition, _rightParen, block, elsePart) {
    if (condition.eval()) {
      block.eval()
    } else {
      elsePart.eval()
    }
  },
  Statement_while(_while, _leftParen, condition, _rightParen, block) {
    while (condition.eval()) {
      try {
        block.eval()
      } catch (e) {
        if (e === "break") break
        throw e
      }
    }
  },
  Block(_leftBrace, statements, _rightBrace) {
    statements.children.forEach(statement => statement.eval())
  },
  ElsePart_else(_else, blockOrIf) {
    blockOrIf.eval()
  },
  Exp_binary(left, op, right) {
    const [x, y] = [left.eval(), right.eval()]
    return op.sourceString == "+" ? x + y : x - y
  },
  RelExp_relational(left, op, right) {
    const [x, y] = [left.eval(), right.eval()]
    switch (op.sourceString) {
      case "<": return x < y;
      case ">": return x > y;
      case "<=": return x <= y;
      case ">=": return x >= y;
      case "==": return x == y;
      case "!=": return x != y;
      default: throw new Error("Unknown relational operator");
    }
  },
  Term_binary(left, op, right) {
    const [x, o, y] = [left.eval(), op.sourceString, right.eval()]
    return o == "*" ? x * y : o == "/" ? x / y : x % y
  },
  Factor_binary(left, _op, right) {
    return left.eval() ** right.eval()
  },
  Primary_parens(_leftParen, e, _rightParen) {
    return e.eval()
  },
  Primary_num(num) {
    return Number(num.sourceString)
  },
  Primary_id(id) {
    const entity = memory[id.sourceString]
    check(entity !== undefined, `${id.sourceString} not defined`, id)
    check(entity?.type === "NUM", `Expected type number`, id)
    return entity.value
  },
  Primary_call(id, _open, exps, _close) {
    const entity = memory[id.sourceString]
    check(entity !== undefined, `${id.sourceString} not defined`, id)
    check(entity?.type === "FUNC", "Function expected", id)
    const args = exps.asIteration().children.map(e => e.eval())
    check(args.length === entity?.paramCount, "Wrong number of arguments", exps)
    return entity.value(...args)
  },
})

try {
  const match = astroGrammar.match(process.argv[2])
  if (match.failed()) throw new Error(match.message)
  console.log(evaluator(match).eval())
} catch (e) {
  console.error(`${e}`)
}