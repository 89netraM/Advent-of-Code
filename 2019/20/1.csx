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

IImmutableDictionary<string, IImmutableList<Coord>> portalLocations;
IImmutableDictionary<Coord, string> portalName;
{
	ImmutableDictionary<string, IImmutableList<Coord>>.Builder pLBuilder = ImmutableDictionary.CreateBuilder<string, IImmutableList<Coord>>();
	ImmutableDictionary<Coord, string>.Builder pNBuilder = ImmutableDictionary.CreateBuilder<Coord, string>();

	foreach (Coord coord in map)
	{
		foreach (Coord dir in Directions)
		{
			char neighborChar = inputLines[coord.y + dir.y][coord.x + dir.x];
			if (Char.IsLetter(neighborChar))
			{
				char nextNeighborChar = inputLines[coord.y + dir.y * 2][coord.x + dir.x * 2];
				if (Char.IsLetter(nextNeighborChar))
				{
					string name = dir switch
					{
						Coord(0, -1) or Coord(-1, 0) => String.Concat(nextNeighborChar, neighborChar),
						Coord(1, 0) or Coord(0, 1) => String.Concat(neighborChar, nextNeighborChar),
						_ => throw new Exception("Unknown direction!"),
					};
					if (pLBuilder.TryGetValue(name, out IImmutableList<Coord> connectedCoords))
					{
						pLBuilder[name] = connectedCoords.Add(coord);
					}
					else
					{
						pLBuilder.Add(name, ImmutableList.Create(coord));
					}
					pNBuilder.Add(coord, name);
				}
			}
		}
	}

	portalLocations = pLBuilder.ToImmutable();
	portalName = pNBuilder.ToImmutable();
}

IList<Coord> PossibleMoves(Coord from)
{
	IList<Coord> possibleMoves = Directions
		.Select(d => new Coord(from.x + d.x, from.y + d.y))
		.Where(c => map.Contains(c))
		.ToList();
	if (portalName.TryGetValue(from, out string portal))
	{
		foreach (Coord other in portalLocations[portal])
		{
			if (other != from)
			{
				possibleMoves.Add(other);
			}
		}
	}
	return possibleMoves;
}

HashSet<Coord> visited = new();
Queue<(Coord, int)> toVisit = new(portalLocations["AA"].Select(static c => (c, 0)));

while (toVisit.Count > 0)
{
	var (current, distance) = toVisit.Dequeue();
	visited.Add(current);
	foreach (Coord next in PossibleMoves(current))
	{
		if (portalName.TryGetValue(next, out string portal) && portal == "ZZ")
		{
			WriteLine(distance + 1);
			return;
		}
		else if (!visited.Contains(next))
		{
			toVisit.Enqueue((next, distance + 1));
		}
	}
}
