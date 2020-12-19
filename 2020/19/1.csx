using System.Text.RegularExpressions;

class Rule
{
	public static Dictionary<string, Rule> Rules;

	string Literal;
	IEnumerable<IEnumerable<string>> Alts;

	public Rule(string literal) => Literal = literal;
	public Rule(IEnumerable<IEnumerable<string>> alts) => Alts = alts;

	public int Check(string s, int index = 0)
	{
		if (Literal is null)
		{
			foreach (var alt in Alts)
			{
				bool match = true;
				int localIndex = index;
				foreach (var rule in alt)
				{
					int retI = Rules[rule].Check(s, localIndex);
					if (retI != -1)
					{
						localIndex = retI;
					}
					else
					{
						match = false;
						break;
					}
				}
				if (match)
				{
					return localIndex;
				}
			}
			return -1;
		}
		else
		{
			return s.Substring(index).StartsWith(Literal) ? index + Literal.Length : -1;
		}
	}
}

Regex literalRegex = new Regex("^\"(\\w+)\"");
Rule MakeRule(string s)
{
	Match literalM = literalRegex.Match(s);
	if (literalM.Success)
	{
		return new Rule(literalM.Groups[1].Value);
	}
	else
	{
		return new Rule(s.Split("|").Select(p => p.Split(" ").Where(s => !String.IsNullOrWhiteSpace(s))));
	}
}

var input = File.ReadAllText("input.txt")
	.Split("\n\n")
	.ToArray();
Rule.Rules = input[0]
	.Split("\n")
	.Select(l => l.Split(": "))
	.ToDictionary(p => p[0], p => MakeRule(p[1]));

WriteLine(
	input[1]
		.Split("\n")
		.Sum(l => Rule.Rules["0"].Check(l) == l.Length ? 1 : 0)
);