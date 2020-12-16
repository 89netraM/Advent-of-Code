using System.Text.RegularExpressions;

Regex nameRegex = new Regex(@"(.*?):");
Regex ruleRegex = new Regex(@"(\d+)-(\d+)");
(string name, IEnumerable<Func<int, bool>> tests) CreateRule(string l)
{
	string name = nameRegex.Match(l).Groups[1].Value;

	IList<Func<int, bool>> tests = new List<Func<int, bool>>();
	foreach (Match match in ruleRegex.Matches(l))
	{
		int lo = Int32.Parse(match.Groups[1].Value);
		int hi = Int32.Parse(match.Groups[2].Value);
		tests.Add(i => lo <= i && i <= hi);
	}

	return (name, tests);
}

var blocks = File.ReadAllText("input.txt")
	.Split("\n\n")
	.ToArray();
var rules = blocks[0]
	.Split("\n")
	.Select(CreateRule)
	.ToList();

var validOthers = blocks[2]
	.Split("\n")
	.Skip(1)
	.Select(s => s.Split(",").Select(Int32.Parse).ToArray())
	.Where(ii => !ii.Any(i => !rules.Any(ff => ff.tests.Any(f => f(i)))));

IDictionary<int, ISet<string>> map = new Dictionary<int, ISet<string>>();
int length = validOthers.First().Length;
for (int i = 0; i < length; i++)
{
	map[i] = rules.Select(r => r.name).ToHashSet();
}
foreach (var other in validOthers)
{
	for (int i = 0; i < other.Length; i++)
	{
		foreach (var rule in rules)
		{
			if (!rule.tests.Any(f => f(other[i])))
			{
				map[i].Remove(rule.name);
			}
		}
	}
}

IDictionary<string, int> ruleLocations = new Dictionary<string, int>();
foreach (var kvp in map.OrderBy(s => s.Value.Count))
{
	foreach (var rule in rules)
	{
		if (!ruleLocations.ContainsKey(rule.name) && kvp.Value.Contains(rule.name))
		{
			ruleLocations[rule.name] = kvp.Key;
			break;
		}
	}
}

var my = blocks[1]
	.Split("\n")
	.Skip(1)
	.First()
	.Split(",")
	.Select(Int32.Parse)
	.ToArray();

WriteLine(
	ruleLocations
		.Where(kvp => kvp.Key.StartsWith("departure"))
		.Aggregate((long)1, (a, kvp) => a * my[kvp.Value])
);