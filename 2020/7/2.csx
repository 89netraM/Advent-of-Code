using System.Text.RegularExpressions;

Regex outer = new Regex(@"(.+?) bags");
Regex inner = new Regex(@"(\d+) ([^,]+) bag");

(string from, IEnumerable<(int count, string name)> to) MakeRules(string line)
{
	Match outerMatch = outer.Match(line);
	string outerBag = outerMatch.Groups[1].Value;

	List<(int count, string name)> tos = new List<(int count, string name)>();
	foreach (Match innerMatch in inner.Matches(line))
	{
		tos.Add((Int32.Parse(innerMatch.Groups[1].Value), innerMatch.Groups[2].Value));
	}
	return (outerBag, tos);
}

var dict = File.ReadAllLines("input.txt")
	.Select(MakeRules)
	.ToDictionary(p => p.from, p => p.to);

int CountChildren(string k)
{
	if (dict.ContainsKey(k))
	{
		return dict[k].Sum(k => k.count * (1 + CountChildren(k.name)));
	}
	else
	{
		return 0;
	}
}

WriteLine(
	CountChildren("shiny gold")
);