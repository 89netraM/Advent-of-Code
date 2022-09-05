using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2015
{
	[Day(15)]
	public class Day15
	{
		[Part(1)]
		public long Part1(string input)
		{
			var ingredients = input
				.Lines()
				.Extract<Ingredient>(@"capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)")
				.ToArray();

			return CalculatePossibleScores(ingredients, 100)
				.Max(i => i.Score);
		}

		[Part(2)]
		public long Part2(string input)
		{
			var ingredients = input
				.Lines()
				.Extract<Ingredient>(@"capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)")
				.ToArray();

			return CalculatePossibleScores(ingredients, 100)
				.Where(i => i.Calories == 500)
				.Max(i => i.Score);
		}

		private IEnumerable<Ingredient> CalculatePossibleScores(Ingredient[] ingredients, long teaspoons)
		{
			if (ingredients.Length == 1)
			{
				yield return ingredients[0] * teaspoons;
			}
			else
			{
				for (long t = 1; t < teaspoons; t++)
				{
					var ingredient = ingredients[0] * t;
					foreach (var other in CalculatePossibleScores(ingredients[1..], teaspoons - t))
					{
						yield return ingredient + other;
					}
				}
			}
		}

		private record Ingredient(long Capacity, long Durability, long Flavor, long Texture, long Calories)
		{
			public long Score =>
				NegativeToZero(Capacity) *
					NegativeToZero(Durability) *
					NegativeToZero(Flavor) *
					NegativeToZero(Texture);

			private static long NegativeToZero(long l) => l < 0 ? 0 : l;

			public static Ingredient operator *(Ingredient i, long f) =>
				new(i.Capacity * f,
					i.Durability * f,
					i.Flavor * f,
					i.Texture * f,
					i.Calories * f);

			public static Ingredient operator +(Ingredient a, Ingredient b) =>
				new(a.Capacity + b.Capacity,
					a.Durability + b.Durability,
					a.Flavor + b.Flavor,
					a.Texture + b.Texture,
					a.Calories + b.Calories);
		}
	}
}
