module textbasedfileformats.json;

import std.algorithm.comparison : among, min;
import std.algorithm.searching : findAmong, startsWith;
import std.algorithm.iteration : filter;
import std.array : appender, empty;
import std.ascii : isDigit;
import std.bigint;
import std.conv : to;
import std.exception : enforce;
import std.format : format;
import std.stdio;
import std.string : representation;
import std.sumtype;
import std.math : isClose, isNaN, isInfinity, pow;
import std.uni : isWhite;

@safe:

alias Payload = SumType!
	( string
	, long
	, bool
	, double
	, typeof(null)
	, BigInt
	, This[]
	, This[string]);

@safe unittest {
	Payload v = 10;
}

private immutable tokenEnd = [ ' ', '\t', '\r', '\n', ',', ':', '{', '}', '[', ']'];
private immutable whiteSpace = [ ' ', '\t'];

private bool isTokenStop(char c) pure @safe @nogc {
	foreach(it; tokenEnd) {
		if(c == it) {
			return true;
		}
	}
	return false;
}

private double exp10(int exp) pure @trusted @nogc {
    enum min = -19;
    enum max = 19;
    static __gshared immutable expmuls = {
        double[max - min + 1] ret;
        double m = 0.1;
        foreach_reverse (i; min .. 0) { ret[i-min] = m; m *= 0.1; }
        m = 1.0;
        foreach (i; 0 .. max) { ret[i-min] = m; m *= 10.0; }
        return ret;
    }();
    if (exp >= min && exp <= max) return expmuls[exp-min];
    return 10.0 ^^ exp;
}

private bool matches(string toMatch)(string input) {
	const bool sw = input.startsWith(toMatch);
	if(!sw) {
		return false;
	}

	const rest = input[toMatch.length .. $];
	if(rest.empty) {
		return true;
	}

	foreach(it; tokenEnd) {
		if(rest[0] == it) {
			return true;
		}
	}
	return false;
}

unittest {
	string tt = "null, true";
	assert(tt.matches!("null"));
}

Payload parseJson(string input) {
	JsonParser jp = JsonParser(input);
	return jp.parse();
}

struct JsonParser {
@safe pure:
	string input;
	size_t row;
	size_t column;

	this(string input) {
		this.input = input;
	}

	void stripWhitespace() {
		outer: while(!this.input.empty) {
			if(this.input[0] == '\n') {
				this.input = this.input[1 .. $];
				this.row++;
				this.column = 1;
				continue outer;
			}
			foreach(it; whiteSpace) {
				if(this.input[0] == it) {
					this.column++;
					this.input = this.input[1 .. $];
					continue outer;
				}
			}
			return;
		}
	}

	Payload parse() {
		return parseElement();
	}

	Payload parseElement() {
		return this.parseValue;
	}

	Payload parseValue() {
		this.stripWhitespace();
		if(this.input.startsWith("{")) {
			this.input = this.input[1 .. $];
			this.column++;
			return this.parseObject();
		} else if(this.input.startsWith("[")) {
			this.input = this.input[1 .. $];
			this.column++;
			return this.parseArray();
		} else if(this.input.startsWith("\"")) {
			return Payload(this.parseString());
		} else if(matches!("null")(this.input)) {
			this.column += 4;
			this.input = this.input[4 .. $];
			Payload ret;
			() @trusted {
				ret = null;
			}();
			return ret;
		} else if(matches!("false")(this.input)) {
			this.column += 5;
			this.input = this.input[5 .. $];
			return Payload(false);
		} else if(matches!("true")(this.input)) {
			this.column += 4;
			this.input = this.input[4 .. $];
			return Payload(false);
		} else if(this.input[0] >= '0' || this.input[0] <= '9'
				|| this.input[0] == '-' 
				|| this.input[0] == '+')
		{
			return this.parseNumber();
		}
		throw new Exception("Failed to parse Payload, startsWith "
				~ this.input[0 .. min(this.input.length, 10)]);
	}

	Payload parseNumber() {
        enforce(!input.empty, "Passed empty range to parseNumber");

		BigInt collector;
        bool neg = false;

		Payload returnInt() {
			collector = neg
				? -collector
				: collector;

			const long r = collector.toLong();
			return r == long.max || r == long.min
				? Payload(collector)
				: Payload(r);
		}

        // negative sign
        if (input[0] == '-')
        {
			this.input = this.input[1 .. $];
			this.column++;
            neg = true;
        }

        enforce(!input.empty, "Input empty before integer parsing started");
        if (input[0] == 'I') {
			enum inf = "Infinity";
            if (input.matches!(inf)())
            {
                this.column += inf.length;
				this.input = this.input[inf.length .. $];
                return Payload(neg ? -double.infinity : double.infinity);
            }
            enforce(false, "Invalid number, expected 'Infinity'");
        }
        if (!neg && input[0] == 'N')
        {
			enum nan = "NaN";
            if (input.matches!("NaN")())
            {
                this.column += nan.length;
				this.input = this.input[nan.length .. $];
				return Payload(double.nan);
            }
            enforce(false, "Invalid number, expected 'NaN'");
        }
        
        // integer part of the number
		enforce(!input.empty || input[0].isDigit(), "Invalid number, expected digit '"
				~ input[0 .. min(10, input.length)] ~ "'");

        if (input[0] == '0')
        {
			this.input = this.input[1 .. $];
			this.column++;
            if (input.empty) // return 0
            {
				long r = 0;
                return Payload(r);
            }

			//enforce(!input[0].isDigit() || isTokenStop(input[0]), 
			//	"Invalid number, 0 must not be followed by another digit '"
			//	~ input[0 .. min(10, input.length)] ~ "'");
        }

        while (!input.empty && isDigit(input[0])) {
            collector = collector * 10 + (input[0] - '0');
			this.input = this.input[1 .. $];
			this.column++;

            if (input.empty || isTokenStop(input[0])) // return integer
            {
                return returnInt();
            }
        }

        int exponent = 0;

        // post decimal point part
        enforce(!input.empty);
        if (input[0] == '.')
        {
			this.input = this.input[1 .. $];
			this.column++;

			enforce(!input.empty || input[0].isDigit()
					, "Invalid number, expected digit '"
					~ input[0 .. min(10, input.length)] ~ "'");

            while (true)
            {
                uint digit = input[0] - '0';
                if (digit > 9) {
					break;
				}

                collector = collector * 10 + digit;
                exponent--;
				this.input = this.input[1 .. $];
				this.column++;

                if (input.empty || isTokenStop(input[0])) {
					BigInt integralPart = collector / pow(10, -exponent);
					integralPart = integralPart * pow(10, -exponent);
					BigInt floatPart = collector - integralPart;
					integralPart = integralPart / pow(10, -exponent);
					string d = integralPart.toDecimalString() ~ "." ~
						floatPart.toDecimalString();
					return Payload(d.to!double());
                }
            }

			enforce(exponent != 0, "Missing fractional number part");
        }

        // exponent
        enforce(!input.empty);
        if (input[0] == 'e' || input[0] == 'E')
        {
			this.input = this.input[1 .. $];
			this.column++;
			enforce(!input.empty, "Missing exponent");

            bool negexp = void;
            if (input[0] == '-')
            {
                negexp = true;
				this.input = this.input[1 .. $];
				this.column++;
            }
            else
            {
                negexp = false;
                if (input[0] == '+') {
					this.input = this.input[1 .. $];
					this.column++;
				}
            }

			enforce(!input.empty || !input[0].isDigit, "Missing exponent");

            uint exp = 0;
            while (true)
            {
                exp = exp * 10 + (input[0] - '0');
				this.input = this.input[1 .. $];
				this.column++;
                if (input.empty || !input[0].isDigit()) {
					break;
				}
            }

            if (negexp) {
				exponent -= exp;
			}
            else  {
				exponent += exp;
			}
        }

		if (exponent == 0) {
			return returnInt();
		}

        if (neg) {
			collector = -collector;
		}

        //_front.number = exp10(exponent) * int_part.toDecimalString.to!double;
        return Payload(collector.toDecimalString.to!double() * exp10(exponent));
	}

	Payload parseObject() {
		Payload[string] elements;
		bool notFirst = false;
		this.stripWhitespace();
		while(!this.input.empty && this.input[0] != '}') {
			if(notFirst) {
				this.stripWhitespace();
				enforce(this.input[0] == ',', "Expected ',' got '" ~ this.input[0]
						~ "'");
				this.input = this.input[1 .. $];
				this.column++;
			}
			this.stripWhitespace();
			string key = this.parseString();
			this.stripWhitespace();
			enforce(this.input[0] == ':', "Expected ':' got '" ~ this.input[0]
					~ "'");
			this.input = this.input[1 .. $];
			this.column++;
			this.stripWhitespace();
			Payload value = this.parseElement();
			() @trusted {
				elements[key] = value;
			}();
			notFirst = true;
			this.stripWhitespace();
		}
		this.stripWhitespace();
		enforce(this.input[0] == '}', "Expected '}' got '" ~ this.input[0]
				~ "'");
		this.column++;
		return Payload(elements);
	}

	Payload parseArray() {
		Payload[] elements;
		bool notFirst = false;
		this.stripWhitespace();
		while(!this.input.empty && this.input[0] != ']') {
			if(notFirst) {
				this.stripWhitespace();
				enforce(this.input[0] == ',', "Expected ',' got '" ~ this.input[0]
						~ "'");
				this.input = this.input[1 .. $];
				this.column++;
				this.stripWhitespace();
			}
			this.stripWhitespace();
			elements ~= this.parseElement();
			this.stripWhitespace();
			notFirst = true;
		}
		this.stripWhitespace();
		enforce(this.input[0] == ']', "Expected ']' got '" ~ this.input[0]
				~ "'");
		this.column++;
		this.input = this.input[1 .. $];
		this.stripWhitespace();
		return Payload(elements);
	}

	string parseString() {
		enforce(this.input[0] == '"', "Expected '\"' got '" ~ this.input[0]
				~ "'");
		this.input = this.input[1 .. $];
		string copy = this.input;
		size_t idx = 1;
		while(idx < this.input.length) {
			if(idx > 1 && this.input[idx] == '"' && this.input[idx - 1] != '\\') {
				break;
			}
			if(idx == 1 && this.input[idx] == '"') {
				break;
			}
			++idx;
		}

		string ret = copy[0 .. idx];
		this.input = this.input[idx+1 .. $];
		this.column += idx + 1;
		return ret;
	}
}

unittest {
	string tp = `{}`;
	auto p = parseJson(tp);
}

unittest {
	string tp = `{ "hello": null }`;
	auto p = parseJson(tp);
}

unittest {
	string tp = `{ "hello": [] }`;
	auto p = parseJson(tp);
}

unittest {
	string tp = `{ "hello": "world" }`;
	auto p = parseJson(tp);
}

unittest {
	string tp = `{ "hello": [ null, true, false] }`;
	auto p = parseJson(tp);
}

unittest {
	string tp = `{ "hello": [ null , 
		true, false
	]}`;
	auto p = parseJson(tp);
}

private string numberToString(Payload p) @safe pure{
	return p.match!
			( (long l) => l.to!string()
			, (double d) => format("%f", d)
			, (_) => "Not a number"
			);
}

@safe unittest
{
    import core.exception;
    import std.exception;

	@safe void test(V)(string input, V expected, string rest, int line = __LINE__) {
		Payload rslt = expected;
		Payload r;
		auto p = JsonParser(input);
		try {
			() @trusted {
				r = p.parseNumber();
			}();
		} catch(Exception e) {
			throw new AssertError(e, __FILE__, line);
		}
		() @trusted {
			bool okay = r.match!
				( (long a) => rslt.match!
				  	( (long b) {
						return a == b;
					  }
					, (double b) => false
					, (_) => false
					)
				, (double a) => rslt.match!
				  	( (long b) => false
					, (double b) {
						return isNaN(a) 
							? isNaN(a) == isNaN(b)
							: isInfinity(a)
								? isInfinity(a) == isInfinity(b)
								: isClose(a, b);
					  }
					, (_) => false
					)
				, (_) => false
				);
			if(!okay) {
				string expStr = numberToString(rslt);
				string gotStr = numberToString(r);
				throw new AssertError(format("input %s\nexp: %s\ngot: %s"
							, input, expStr, gotStr), __FILE__, line);
			}
		}();
	}

    test("NaN", double.nan, "");
    test("NaN ", double.nan, " ");
    test("Infinity", double.infinity, "");
    test("Infinity ", double.infinity, " ");
    test("-Infinity", -double.infinity, "");
    test("-Infinity ", -double.infinity, " ");
    test("0", 0, "");
    test("0 ", 0, " ");
    test("12", 12, "");
    test("12 ", 12, " ");
    test("1249", 1249, "");
    test("1249 ", 1249, " ");
    test("123", 123, "");
    test("123.0", 123.0, "");
    test("123.0 ", 123.0, " ");
    test("123.456", 123.456, "");
    test("123.456 ", 123.456, " ");
    test("-0", 0, "");
    test("-0 ", 0, " ");
    test("-0e+10 ", 0.0, " ");
    test("123.456e1", 1234.56, "");
    test("123.456e1 ", 1234.56, " ");
    test("123.456e+1", 1234.56, "");
    test("123.456e+1 ", 1234.56, " ");
    test("123.456e-1", 12.3456, "");
    test("123.456e-1 ", 12.3456, " ");
    test("123.456e-01", 12.3456, "");
    test("123.456e-01 ", 12.3456, " ");
    test("0.123e-12", 0.123e-12, "");
    test("0.123e-12 ", 0.123e-12, " ");
}

@trusted unittest {
	import std.file;
	foreach(ma; dirEntries("JSONTestSuite/test_parsing/", SpanMode.depth)
			.filter!(n => n.name.startsWith("JSONTestSuite/test_parsing/y_"))) 
	{
		writeln(ma.name);
		auto p = parseJson(readText(ma.name));
	}
}
