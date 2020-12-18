#r "nuget: Prat, 1.1.0"
using Prat;

IParser<long> Val = Parsers.Common.Integer().Select(i => (long)i).Or(
	Parsers.KeepLeft(
		Parsers.Common.Char('(').And(() => Expression),
		Parsers.Common.Char(')')
	)
);
IParser<long> Op = Parsers.Using(
	Val,
	first => Parsers.Using(
		Parsers.Common.Char('+').Or(Parsers.Common.Char('*')),
		op => Val.Select(l => (op, l))
	).ZeroOrMore()
	.Select(opls => opls.Aggregate(first, (a, opl) => opl.op == '+' ? a + opl.l : a * opl.l))
);
IParser<long> Expression = Op.Or(Val);
long Calculate(string line)
{
	var result = Expression.Parse(line.Replace(" ", ""));
	return result.Value.Item1;
}

WriteLine(
	File.ReadAllLines("input.txt")
		.Select(Calculate)
		.Sum()
);