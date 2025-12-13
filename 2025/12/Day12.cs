using System;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2025;

[Day(12)]
public class Day12
{
	[Part(1)]
	public object Part1(string input)
	{
		if (input.Paragraphs() is not [.. var shapesInput, var regionsInput])
		{
			throw new ArgumentException("Unknown input format", nameof(input));
		}
		var shapes = shapesInput.Select(s => s.Count(c => c is '#')).ToArray();
		return regionsInput.Lines()
			.Extract<(long, long, string)>(@"^(\d+)x(\d+): (.*)$")
			.Select(t => (width: t.Item1, height: t.Item2, shapeCounts: t.Item3.Words().Select(long.Parse).ToArray()))
			.Count(region =>
			{
				var minimumShapeArea = region.shapeCounts.Select((count, i) => count * shapes[i]).Sum();
				var regionArea = region.width * region.height;
				if (minimumShapeArea > regionArea)
				{
					return false;
				}

				var shapeCount = region.shapeCounts.Sum();
				var regionShapeCount = (region.width / 3L) * (region.height / 3L);
				if (regionShapeCount >= shapeCount)
				{
					return true;
				}

				throw new Exception("Unapproximatable!");
			});
	}
}
