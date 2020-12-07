using System.Text.RegularExpressions;

Regex outer = new Regex(@"(.+?) bags");
Regex inner = new Regex(@"(\d+) ([^,]+) bag");

(string from, IEnumerable<string> to) MakeRules(string line)
{
	Match outerMatch = outer.Match(line);
	string outerBag = outerMatch.Groups[1].Value;

	List<string> tos = new List<string>();
	foreach (Match innerMatch in inner.Matches(line))
	{
		tos.Add(innerMatch.Groups[2].Value);
	}
	return (outerBag, tos);
}

var dict = File.ReadAllLines("input.txt")
	.Select(MakeRules)
	.ToDictionary(p => p.from, p => p.to);

bool ContainsGold(string k)
{
	if (dict.ContainsKey(k))
	{
		return dict[k].Contains("shiny gold") || dict[k].Any(ContainsGold);
	}
	else
	{
		return false;
	}
}

WriteLine(
	dict.Sum(kvp => ContainsGold(kvp.Key) ? 1 : 0)
);