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

IList<string> leftOutside = new List<string>();
foreach (var food in foods)
{
	foreach (var ingredient in food.ingredients)
	{
		if (!allergensToIngredient.Any(kvp => kvp.Value.Contains(ingredient)))
		{
			leftOutside.Add(ingredient);
		}
	}
}

WriteLine(
	leftOutside.Count
);