////////////
// DOUBLE //
////////////

a = [0.0, 10.0, 10.1, 10.5, 10.9, -10.0, -10.1, -10.5, -10.9];
b = [0.0, 6.0, 6.1, 6.5, 6.9, -6.0, -6.1, -6.5, -6.9, 11.1, -11.1];

// unary
u = [ \abs, \ceil, \floor, \frac, \sign, \squared, \sqrt, \exp, \midicps, \cpsmidi, \midiratio, \ratiomidi, \dbamp, \ampdb, \octcps, \cpsoct, \log, \log2, \log10, \sin, \cos, \tan, \asin, \acos, \atan, \sinh, \cosh, \tanh, \cubed, \reciprocal];

n = [\log2].asSet;

f = { |x|
	var s = x.asString;
	if (x.isKindOf(Integer) or: s.includes($.) or: s.includes($e) or: s.contains("inf"), s, s ++ ".0");
};

t = "Double";

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
i = [\div, \round, \roundUp, \trunc, \atan2, \hypot, \hypotApx, \pow, \difsqr, \sumsqr, \sqrsum, \sqrdif, \absdif, \clip2, \excess, \fold2, \wrap2, \mod, \ring1, \ring2, \ring3, \ring4, \thresh, \amclip, \scaleneg];

i.do { |sel|
	("\"" ++ t + "." ++ sel ++ "\" should work in {").postln;
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

///////////
// FLOAT //
///////////

f = { |x|
	var s = x.asString;
	if (x.isKindOf(Integer) or: s.contains("inf"), s, {
		if (s.includes($.) or: s.includes($e), s ++ "f", s ++ ".0f");
	});
};

t = "Float";
