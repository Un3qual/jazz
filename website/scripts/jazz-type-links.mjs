const standardLibrary = '/docs/standard-library';
const runtimeValues = '/docs/reference/runtime-values';
const prelude = `${standardLibrary}/prelude`;

export const JAZZ_TYPE_DESTINATIONS = Object.freeze({
  Maybe: `${standardLibrary}/maybe`,
  Result: `${standardLibrary}/result`,
  NonEmpty: `${standardLibrary}/nonempty`,
  Dictionary: `${standardLibrary}/dictionary`,
  Queue: `${standardLibrary}/queue`,
  Map: `${standardLibrary}/map`,
  Set: `${standardLibrary}/set`,
  Char: `${standardLibrary}/char`,
  Text: `${standardLibrary}/text`,
  List: `${standardLibrary}/list`,
  IOError: `${standardLibrary}/io-error#ioerror`,
  IOErrorCategory: `${standardLibrary}/io-error#ioerrorcategory`,
  Ordering: `${prelude}#ordering`,
  Eq: `${prelude}#eq`,
  Ord: `${prelude}#ord`,
  Num: `${prelude}#num`,
  Integral: `${prelude}#integral`,
  Fractional: `${prelude}#fractional`,
  Showable: `${prelude}#showable`,
  Default: `${prelude}#default`,
  Bool: `${runtimeValues}#bool`,
  Int: `${runtimeValues}#int`,
  Float: `${runtimeValues}#float`,
  Int8: `${runtimeValues}#int8`,
  Int16: `${runtimeValues}#int16`,
  Int32: `${runtimeValues}#int32`,
  Int64: `${runtimeValues}#int64`,
  UInt8: `${runtimeValues}#uint8`,
  UInt16: `${runtimeValues}#uint16`,
  UInt32: `${runtimeValues}#uint32`,
  UInt64: `${runtimeValues}#uint64`,
  Float16: `${runtimeValues}#float16`,
  Float32: `${runtimeValues}#float32`,
  Float64: `${runtimeValues}#float64`,
  '[': `${standardLibrary}/list`,
  ']': `${standardLibrary}/list`,
  '(': `${runtimeValues}#tuples`,
  ')': `${runtimeValues}#tuples`,
  '()': `${runtimeValues}#unit`,
});

const listOpenDestination = JAZZ_TYPE_DESTINATIONS['['];
const listCloseDestination = JAZZ_TYPE_DESTINATIONS[']'];
const tupleOpenDestination = JAZZ_TYPE_DESTINATIONS['('];
const tupleCloseDestination = JAZZ_TYPE_DESTINATIONS[')'];
const unitDestination = JAZZ_TYPE_DESTINATIONS['()'];
const jazzIdentifierPattern = /[\p{Alphabetic}_][\p{Alphabetic}\p{Number}_'!]*/gu;

function findBalancedPairs(source, open, close) {
  const opens = [];
  const pairs = [];

  for (let index = 0; index < source.length; index += 1) {
    if (source[index] === open) {
      opens.push(index);
    } else if (source[index] === close) {
      const start = opens.pop();
      if (start !== undefined) {
        pairs.push({start, end: index});
      }
    }
  }

  return pairs;
}

function hasTopLevelComma(source, start, end) {
  let parentheses = 0;
  let brackets = 0;

  for (let index = start; index < end; index += 1) {
    switch (source[index]) {
      case '(':
        parentheses += 1;
        break;
      case ')':
        parentheses -= 1;
        break;
      case '[':
        brackets += 1;
        break;
      case ']':
        brackets -= 1;
        break;
      case ',':
        if (parentheses === 0 && brackets === 0) {
          return true;
        }
        break;
      default:
        break;
    }
  }

  return false;
}

function lexJazzIdentifiers(source) {
  return [...source.matchAll(jazzIdentifierPattern)].map((match) => ({
    start: match.index,
    end: match.index + match[0].length,
    name: match[0],
  }));
}

function addSpan(spans, start, end, destination) {
  spans.push({start, end, destination});
}

function withoutOverlaps(spans) {
  const ordered = [...spans].sort(
    (left, right) =>
      left.start - right.start ||
      left.end - right.end ||
      left.destination.localeCompare(right.destination),
  );
  const result = [];

  for (const span of ordered) {
    if (result.at(-1)?.end <= span.start || result.length === 0) {
      result.push(span);
    }
  }

  return result;
}

export function getJazzTypeLinkSpans(source) {
  const spans = [];
  const identifiers = lexJazzIdentifiers(source);
  const identifierEnds = new Set(identifiers.map(({end}) => end));

  for (const {start, end, name} of identifiers) {
    const destination = JAZZ_TYPE_DESTINATIONS[name];
    if (destination) {
      addSpan(spans, start, end, destination);
    }
  }

  for (const {start, end} of findBalancedPairs(source, '[', ']')) {
    addSpan(spans, start, start + 1, listOpenDestination);
    addSpan(spans, end, end + 1, listCloseDestination);
  }

  for (const {start, end} of findBalancedPairs(source, '(', ')')) {
    const contents = source.slice(start + 1, end);
    const isTypeApplication = identifierEnds.has(start);
    if (!isTypeApplication && /^\s*$/.test(contents)) {
      addSpan(spans, start, end + 1, unitDestination);
    } else if (!isTypeApplication && hasTopLevelComma(source, start + 1, end)) {
      addSpan(spans, start, start + 1, tupleOpenDestination);
      addSpan(spans, end, end + 1, tupleCloseDestination);
    }
  }

  return withoutOverlaps(spans);
}
