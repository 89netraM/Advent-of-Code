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

int takenORE = 0;
var spare = new Dictionary<string, int>();
void Make(string name, int quantity)
{
	if (name == "ORE")
	{
		takenORE += quantity;
	}
	else
	{
		var r = recipes[name];
		int x = (int)Math.Ceiling(quantity / (double)r.To.Quantity);

		foreach (var i in r.From)
		{
			int needed = i.Quantity * x;
			if (spare.ContainsKey(i.Name))
			{
				int toTake = Math.Min(needed, spare[i.Name]);
				needed -= toTake;
				spare[i.Name] -= toTake;
			}
			Make(i.Name, needed);
		}

		spare[name] = (spare.ContainsKey(name) ? spare[name] : 0) + (r.To.Quantity * x - quantity);
	}
}

Make("FUEL", 1);
WriteLine(takenORE);