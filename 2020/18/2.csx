#r "nuget: Prat, 1.1.0"
using Prat;

IParser<long> Val = Parsers.Common.Integer().Select(i => (long)i).Or(
	Parsers.KeepLeft(
		Parsers.Common.Char('(').And(() => Expression),
		Parsers.Common.Char(')')
	)
);
IParser<long> Add = Parsers.Chain(Val, Parsers.Common.Char('+')).Select(all => all.Aggregate((a, l) => a + l));
IParser<long> Mul = Parsers.Chain(Add, Parsers.Common.Char('*')).Select(all => all.Aggregate((a, l) => a * l));
IParser<long> Expression = Mul.Or(Val);
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