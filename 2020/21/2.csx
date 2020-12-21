(ISet<string> ingredients, ISet<string> allergens) MakeFood(string l)
{
	int para = l.IndexOf("(");
	return (
		l.Substring(0, para - 1).Split(" ").ToHashSet(),
		l.Substring(para + 10, l.Length - (para + 11)).Split(", ").ToHashSet()
	);
}

var foods = File.ReadAllLines("input.txt").Select(MakeFood);

IDictionary<string, ISet<string>> allergensToIngredient = new Dictionary<string, ISet<string>>();
foreach (var food in foods)
{
	foreach (var otherFood in foods)
	{
		if (food != otherFood)
		{
			var matches = food.ingredients.Intersect(otherFood.ingredients).ToHashSet();
			var matchingAllergens = food.allergens.Intersect(otherFood.allergens);
			foreach (var allergen in matchingAllergens)
			{
				if (allergensToIngredient.ContainsKey(allergen))
				{
					allergensToIngredient[allergen] = allergensToIngredient[allergen].Intersect(matches).ToHashSet();
				}
				else
				{
					allergensToIngredient[allergen] = matches;
				}
			}
		}
	}
}

while (allergensToIngredient.Any(kvp => kvp.Value.Count > 1))
{
	foreach (var kvp in allergensToIngredient)
	{
		if (kvp.Value.Count > 1)
		{
			foreach (var kvp2 in allergensToIngredient)
			{
				if (kvp.Key != kvp2.Key && kvp2.Value.Count == 1)
				{
					kvp.Value.ExceptWith(kvp2.Value);
				}
			}
		}
	}

}

WriteLine(
	String.Join(
		",",
		allergensToIngredient.OrderBy(kvp => kvp.Key).Select(kvp => kvp.Value.First())
	)
)