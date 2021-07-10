using System.Collections.Immutable;

record Coord(int x, int y);
IImmutableList<Coord> Directions = ImmutableList.Create(new Coord[] { new(0, -1), new(-1, 0), new(1, 0), new(0, 1) });

IImmutableList<string> inputLines = File.ReadAllLines("input.txt").ToImmutableList();
IImmutableSet<Coord> map = inputLines
	.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Coord(x, y)))
		.Where(static p => p.c == '.')
		.Select(static p => p.coord)
	)
	.ToImmutableHashSet();

record Portal(string name, bool isOuter);
IImmutableDictionary<Portal, Coord> portalLocations;
IImmutableDictionary<Coord, Portal> portalName;
{
	ImmutableDictionary<Portal, Coord>.Builder pLBuilder = ImmutableDictionary.CreateBuilder<Portal, Coord>();
	ImmutableDictionary<Coord, Portal>.Builder pNBuilder = ImmutableDictionary.CreateBuilder<Coord, Portal>();

	foreach (Coord coord in map)
	{
		foreach (Coord dir in Directions)
		{
			char neighborChar = inputLines[coord.y + dir.y][coord.x + dir.x];
			if (Char.IsLetter(neighborChar))
			{
				Coord nextNeighbor = new(coord.x + dir.x * 2, coord.y + dir.y * 2);
				char nextNeighborChar = inputLines[nextNeighbor.y][nextNeighbor.x];
				if (Char.IsLetter(nextNeighborChar))
				{
					bool isOuter = 0 == nextNeighbor.y || nextNeighbor.y + 1 == inputLines.Count ||
						0 == nextNeighbor.x || nextNeighbor.x + 1 == inputLines[nextNeighbor.y].Length;
					string name = dir switch
					{
						Coord(0, -1) or Coord(-1, 0) => String.Concat(nextNeighborChar, neighborChar),
						Coord(1, 0) or Coord(0, 1) => String.Concat(neighborChar, nextNeighborChar),
						_ => throw new Exception("Unknown direction!"),
					};
					Portal portal = new(name, isOuter);

					pLBuilder.Add(portal, coord);
					pNBuilder.Add(coord, portal);
				}
			}
		}
	}

	portalLocations = pLBuilder.ToImmutable();
	portalName = pNBuilder.ToImmutable();
}

IList<(Coord, int)> PossibleMoves(Coord from, int level)
{
	IList<(Coord, int)> possibleMoves = Directions
		.Select(d => (c: new Coord(from.x + d.x, from.y + d.y), level))
		.Where(p => map.Contains(p.c))
		.ToList();
	if (portalName.TryGetValue(from, out Portal portal) && portalLocations.TryGetValue(new(portal.name, !portal.isOuter), out Coord other))
	{
		int nextLevel = level + (portal.isOuter ? -1 : 1);
		if (other != from && nextLevel >= 0)
		{
			possibleMoves.Add((other, nextLevel));
		}
	}
	return possibleMoves;
}

Coord start = portalLocations[new("AA", true)];
HashSet<(Coord, int)> visited = new();
visited.Add((start, 0));
Queue<(Coord, int, int)> toVisit = new();
toVisit.Enqueue((start, 0, 0));

while (toVisit.Count > 0)
{
	var (current, distance, level) = toVisit.Dequeue();
	foreach (var (next, nextLevel) in PossibleMoves(current, level))
	{
		if (portalName.TryGetValue(next, out Portal portal) && portal.name == "ZZ")
		{
			if (nextLevel == 0)
			{
				WriteLine(distance + 1);
				return;
			}
		}
		else if (!visited.Contains((next, nextLevel)))
		{
			visited.Add((next, nextLevel));
			toVisit.Enqueue((next, distance + 1, nextLevel));
		}
	}
}
