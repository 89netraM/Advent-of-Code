using System.Text.RegularExpressions;

Regex ruleRegex = new Regex(@"(\d+)-(\d+)");
IEnumerable<Func<int, bool>> CreateRule(string l)
{
	foreach (Match match in ruleRegex.Matches(l))
	{
		int lo = Int32.Parse(match.Groups[1].Value);
		int hi = Int32.Parse(match.Groups[2].Value);
		yield return i => lo <= i && i <= hi;
	}
}

var blocks = File.ReadAllText("input.txt")
	.Split("\n\n")
	.ToArray();
var rules = blocks[0]
	.Split("\n")
	.SelectMany(CreateRule);

var others = blocks[2]
	.Split("\n")
	.Skip(1)
	.SelectMany(s => s.Split(",")
		.Select(Int32.Parse)
		.Where(i => !rules.Any(f => f(i)))
	)
	.Sum();

WriteLine(
	others
);