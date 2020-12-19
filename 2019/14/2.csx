/// <remarks>
/// With inspiration from https://old.reddit.com/r/adventofcode/comments/eafj32/2019_day_14_solutions/faqkkwv/
/// </remarks>

using System.Text.RegularExpressions;

readonly struct Ingredient
{
	public int Quantity { get; }
	public string Name { get; }
	public Ingredient(int quantity, string name) => (Quantity, Name) = (quantity, name);

	public override string ToString() => $"{Quantity} {Name}";
}

readonly struct Recipe
{
	public IEnumerable<Ingredient> From { get; }
	public Ingredient To { get; }

	public Recipe(IEnumerable<Ingredient> from, Ingredient to) => (From, To) = (from, to);
}

Regex fromRegex = new Regex(@"(\d+) (\w+),? ");
Regex toRegex = new Regex(@"=> (\d+) (\w+)");
Recipe CreateRecipe(string l)
{
	IList<Ingredient> from = new List<Ingredient>();
	foreach (Match match in fromRegex.Matches(l))
	{
		from.Add(new Ingredient(Int32.Parse(match.Groups[1].Value), match.Groups[2].Value));
	}

	Match m = toRegex.Match(l);
	return new Recipe(
		from,
		new Ingredient(Int32.Parse(m.Groups[1].Value), m.Groups[2].Value)
	);
}

var recipes = File.ReadAllLines("input.txt")
	.Select(CreateRecipe)
	.ToDictionary(r => r.To.Name, r => r);

long ORE4FUEL(long amount)
{
	long takenORE = 0;
	var spare = new Dictionary<string, long>();
	void Make(string name, long quantity)
	{
		if (name == "ORE")
		{
			takenORE += quantity;
		}
		else
		{
			var r = recipes[name];
			long x = (long)Math.Ceiling(quantity / (double)r.To.Quantity);

			foreach (var i in r.From)
			{
				long needed = i.Quantity * x;
				if (spare.ContainsKey(i.Name))
				{
					long toTake = Math.Min(needed, spare[i.Name]);
					needed -= toTake;
					spare[i.Name] -= toTake;
				}
				Make(i.Name, needed);
			}

			spare[name] = (spare.ContainsKey(name) ? spare[name] : 0) + (r.To.Quantity * x - quantity);
		}
	}
	Make("FUEL", amount);
	return takenORE;
}

long fuel = 1;
long targetORE = 1000000000000;
while (true)
{
	long ore = ORE4FUEL(fuel + 1);
	if (ore > targetORE)
	{
		WriteLine(fuel);
		break;
	}
	else
	{
		fuel = Math.Max(fuel + 1, (fuel + 1) * targetORE / ore);
	}
}