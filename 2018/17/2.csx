using System.Text.RegularExpressions;

record Coord(int x, int y);
static readonly Regex xyRegex = new Regex(@"^x=(\d+), y=(\d+)\.\.(\d+)");
static readonly Regex yxRegex = new Regex(@"^y=(\d+), x=(\d+)\.\.(\d+)");
static IEnumerable<Coord> ParseVein(string line)
{
	Match match = xyRegex.Match(line);
	if (match.Success)
	{
		int x = Int32.Parse(match.Groups[1].Value);
		int y1 = Int32.Parse(match.Groups[2].Value);
		int y2 = Int32.Parse(match.Groups[3].Value);
		for (var y = y1; y <= y2; y++)
		{
			yield return new Coord(x, y);
		}
	}
	else
	{
		match = yxRegex.Match(line);
		if (match.Success)
		{
			int y = Int32.Parse(match.Groups[1].Value);
			int x1 = Int32.Parse(match.Groups[2].Value);
			int x2 = Int32.Parse(match.Groups[3].Value);
			for (var x = x1; x <= x2; x++)
			{
				yield return new Coord(x, y);
			}
		}
	}
}

HashSet<Coord> clay = File.ReadLines("input.txt").SelectMany(ParseVein).ToHashSet();
var minMax = clay.Aggregate((minX: Int32.MaxValue, minY: Int32.MaxValue, maxX: Int32.MinValue, maxY: Int32.MinValue), static (a, c) => (Math.Min(a.minX, c.x), Math.Min(a.minY, c.y), Math.Max(a.maxX, c.x), Math.Max(a.maxY, c.y)));
Dictionary<Coord, bool> water = new Dictionary<Coord, bool>
{
	[new Coord(500, minMax.minY)] = false,
};
Stack<Coord> toVisit = new Stack<Coord>();
toVisit.Push(new Coord(500, minMax.minY));
toVisit.Push(new Coord(500, minMax.minY));

while (toVisit.Count > 0)
{
	Coord coord = toVisit.Pop();
	Coord down = new Coord(coord.x, coord.y + 1);
	if (clay.Contains(down) || (water.TryGetValue(down, out bool isSettled) && isSettled))
	{
		Coord left = new Coord(coord.x - 1, coord.y);
		Coord right = new Coord(coord.x + 1, coord.y);
		if (!water.ContainsKey(left))
		{
			if (clay.Contains(left))
			{
				bool allWater = true;
				int maxX = right.x;
				for (int x = right.x; x <= minMax.maxX; x++)
				{
					Coord c = new Coord(x, coord.y);
					maxX = x;
					if (clay.Contains(c))
					{
						break;
					}
					if (!water.ContainsKey(c))
					{
						allWater = false;
					}
				}
				if (allWater)
				{
					for (int x = coord.x; x < maxX; x++)
					{
						Coord c = new Coord(x, coord.y);
						if (water.ContainsKey(c))
						{
							water[c] = true;
						}
					}
				}
			}
			else
			{
				water.Add(left, false);
				toVisit.Push(left);
				toVisit.Push(left);
			}
		}
		if (!water.ContainsKey(right))
		{
			if (clay.Contains(right))
			{
				bool allWater = true;
				int minX = left.x;
				for (int x = left.x; x >= minMax.minX; x--)
				{
					Coord c = new Coord(x, coord.y);
					minX = x;
					if (clay.Contains(c))
					{
						break;
					}
					if (!water.ContainsKey(c))
					{
						allWater = false;
					}
				}
				if (allWater)
				{
					for (int x = coord.x; x > minX; x--)
					{
						Coord c = new Coord(x, coord.y);
						if (water.ContainsKey(c))
						{
							water[c] = true;
						}
					}
				}
			}
			else
			{
				water.Add(right, false);
				toVisit.Push(right);
				toVisit.Push(right);
			}
		}
	}
	else if (down.y <= minMax.maxY && !water.ContainsKey(down))
	{
		water.Add(down, false);
		toVisit.Push(down);
		toVisit.Push(down);
	}
}

WriteLine(water.Values.Count(static isSettled => isSettled));
