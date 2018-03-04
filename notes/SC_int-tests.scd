/////////
// INT //
/////////

a = [0, 1, -1, 10, -10];
b = [0, 1, -1, 6, -6, 11, -11];

// unary
u = [ \abs, \sign, \squared, \cubed];

n = [].asSet;

f = { |x|
	var s = x.asString;
	s; // if (x.isKindOf(Integer) or: s.includes($.) or: s.includes($e) or: s.contains("inf"), s, s ++ ".0");
};

t = "Int";

u.do { |sel|
	("\"" ++ t ++ "." ++ sel ++ "\" should work in {").postln;
	a.do { |ai|
		if (ai >= 0 or: n.includes(sel).not) {
			var c = ai.perform(sel);
			var s;
			if (c.asString.contains("nan").not) {
				s = f.(c);
				if (c.isKindOf(Integer).not) { s = "tol(" ++ s ++ ")" };
				s = "  " ++ sel ++ "(" ++ f.(ai) ++ ") shouldBe " ++ s;
				s.postln;
			};
		}
	};
	"}\n".postln;
}

// binary
i = [\min, \max, \div, \gcd, \lcm, \clip2, \excess, \fold2, \wrap2, \mod, \difsqr, \sumsqr, \sqrsum, \sqrdif, \absdif, \round, \roundUp, \trunc];

i.do { |sel|
	("\"" ++ t ++ "." ++ sel ++ "\" should work in {").postln;
	a.do { |ai|
		b.do { |bi|
			var c = ai.perform(sel, bi);
			var s;
			if (c.asString.contains("nan").not) {
				s = f.(c);
				if (c.isKindOf(Integer).not) { s = "tol(" ++ s ++ ")" };
				s = "  " ++ sel ++ "(" ++ f.(ai) ++ ", " ++ f.(bi) ++ ") shouldBe " ++ s;
				s.postln;
			};
		}
	};
	"}\n".postln;
}