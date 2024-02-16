import assert from "assert"
import { matches as exMatches } from "../ohm_exercises.js"
import { matches as langMatches } from "../lang.js"

const allMc2 = (() => {
  var out = []
  for (let i = 2221; i <= 2720; ++i) {
    out.push(`${i}999999999999`)
  }
  return out
})()

const testFixture = {
  canadianPostalCode: {
    good: ["A7X 2P8", "P8E 4R2", "K1V 9P2", "Y3J 5C0"],
    bad: ["A7X   9B2", "C7E9U2", "", "Dog", "K1V\t9P2", " A7X 2P8", "A7X 2P8 "],
  },
  visa: {
    good: ["4128976567772613", "4089655522138888", "4098562516243"],
    bad: [
      "43333",
      "42346238746283746823",
      "7687777777263211",
      "foo",
      "π",
      "4128976567772613 ",
    ],
  },
  masterCard: {
    good: [
      "5100000000000000",
      "5294837679998888",
      "5309888182838282",
      "5599999999999999",
      "2221000000000000",
      "2720999999999999",
      "2578930481258783",
      "2230000000000000",
      ...allMc2
    ],
    bad: [
      "5763777373890002",
      "513988843211541",
      "51398884321108541",
      "",
      "OH",
      "5432333xxxxxxxxx",
      "2721999999999999"
    ],
  },
  notThreeEndingInOO: {
    good: ["", "fog", "Tho", "one", "a", "ab", "food"],
    bad: ["fOo", "gOO", "HoO", "zoo", "MOO", "123", "A15"],
  },
  divisibleBy16: {
    good: ["0", "00", "000", "00000", "00000", "000000", "00000000", "1101000000", "10000", "000010000", "111111110000"],
    bad: ["1", "00000000100", "1000000001", "dog0000000"],
  },
  eightThroughThirtyTwo: {
    good: Array(25)
      .fill(0)
      .map((x, i) => i + 8),
    bad: ["1", "0", "00003", "dog", "", "361", "90", "7", "-11", "33"],
  },
  notPythonPycharmPyc: {
    good: ["", "pythons", "pycs", "PYC", "apycharm", "zpyc", "dog", "pythonpyc"],
    bad: ["python", "pycharm", "pyc"],
  },
  restrictedFloats: {
    good: ["1e0", "235e9", "1.0e1", "1.0e+122", "55e20"],
    bad: ["3.5E9999", "2.355e-9991", "1e2210"],
  },
  palindromes2358: {
    good: [
      "aa",
      "bb",
      "cc",
      "aaa",
      "aba",
      "aca",
      "bab",
      "bbb",
      "ababa",
      "abcba",
      "aaaaaaaa",
      "abaaaaba",
      "cbcbbcbc",
    ],
    bad: ["", "a", "ab", "abc", "abbbb", "cbcbcbcb"],
  },
  pyString: {
    good: [
      `"simpleString"`,
      `'anotherSimpleString'`,
      `"""Multi\nLine\nString"""`,
      `'''Another\nMulti\nLine\nString'''`,
      `r"Raw\\nString"`,
      `R'Raw\\String'`,
      `u"Unicode\u1234String"`,
      `U'Unicode\uABCDString'`,
      `f"Formatted{variable}String"`,
      `F'Formatted{variable}String'`,
      `fr"FormattedRaw\\n{variable}String"`,
      `b"bytesString"`,
      `B"bytesString"`,
      `br"RawBytes\\nString"`,
      `rb'RawBytes\String'`,
      `"""Unescaped"InternalQuotes"""`,
      `'''Unescaped'InternalQuotes'''`,
    ],
    bad: [
      `x"InvalidPrefixString"`,
      `"NoEndQuote`,
      `'AnotherNoEndQuote`,
      `"Mismatched'Quotes"`,
      `'Mismatched"Quotes'`,
      `r"Unescaped\RawString`,
      `b"NonAsciiBytes\u1234"`,
      `fr"Unclosed{VariableString`,
      `b'UnfinishedBytes`,
      `R"MismatchedQuotes"""`,
    ]
  }
}

for (let name of Object.keys(testFixture)) {
  describe(`[Problem 1]: when matching ${name}`, () => {
    for (let s of testFixture[name].good) {
      it(`accepts ${s}`, () => {
        assert.ok(exMatches(name, s))
      })
    }
    for (let s of testFixture[name].bad) {
      it(`rejects ${s}`, () => {
        assert.ok(!exMatches(name, s))
      })
    }
  })
}

// TODO: Test allowed chars in identifiers ($, _, @, etc)

const validParseExamples = [
`func add(a, b) a + b end
add[2, 3]`,

`func m$u@l(a, b) a * b end
m$u@l[2, 3]`,

`func factorial(n)
   1 if n == 0 else n * factorial[n - 1]
end
factorial[5]`,

`func concat(str1, str2) str1 + "\n" + str2 end
concat["Hello", "World"]`,

`func compute(a, b, c) (a + b) * (c - 1) end
compute[3, 4, 5]`,

`func square(x) x * x end
func sumSquares(a, b) square[a] + square[b] end
sumSquares[3, 4]`
];

const invalidParseExamples = [
  `square(x) x * x end`, // Function not defined
  `func add(a, b) a + b`, // Missing end
  `func add(a, b) a ++ b end`, // Invalid operator
  `func square(x) x * x end
   square(5)`,  // Should use brackets instead of parentheses
  `func max(a; b) a if a > b else b end`,  // Semicolon instead of comma
  `func compute(a, b) (a + b * 2 end`,  // Unmatched parentheses
  `func greet() "Hello, World end`,  // Missing closing quote
  `func number() 123abc end`  // Alphabetic characters in a numeric literal
];

describe(`NewLang valid parse examples`, () => {
  for (let e of validParseExamples) {
    it(`accepts ${e}`, () => {
      assert.ok(langMatches(e))
    })
  }
})

describe(`NewLang invalid parse examples`, () => {
  for (let e of invalidParseExamples) {
    it(`rejects ${e}`, () => {
      assert.ok(!langMatches(e))
    })
  }
})