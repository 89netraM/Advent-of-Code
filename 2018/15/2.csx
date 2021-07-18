record Coord(int x, int y);
static IEnumerable<Coord> Neighbors(this Coord coord)
{
	yield return new Coord(coord.x, coord.y - 1);
	yield return new Coord(coord.x - 1, coord.y);
	yield return new Coord(coord.x + 1, coord.y);
	yield return new Coord(coord.x, coord.y + 1);
}

record Unit(int hp, bool isGoblin);
static Unit NewGoblin() =>
	new Unit(200, true);
static Unit NewElf() =>
	new Unit(200, false);

string[] lines = File.ReadAllLines("input.txt");

HashSet<Coord> walls = lines.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Coord(x, y))))
	.Where(static p => p.c == '#')
	.Select(static p => p.coord)
	.ToHashSet();

Dictionary<Coord, Unit> startingPlayers = lines.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Coord(x, y))))
	.Where(static p => p.c == 'G' || p.c == 'E')
	.ToDictionary(static p => p.coord, static p => (p.c == 'G' ? NewGoblin() : NewElf()));
Dictionary<Coord, Unit> players = new Dictionary<Coord, Unit>(startingPlayers);

(Coord c, int d)? BreadthFirstSearch(Coord start, bool isLookingForGoblin)
{
	Queue<(Coord, int)> toVisit = new Queue<(Coord, int)>();
	toVisit.Enqueue((start, 1));
	HashSet<Coord> visited = new HashSet<Coord>();
	while (toVisit.Count > 0)
	{
		var (current, length) = toVisit.Dequeue();
		if (!players.ContainsKey(current) && !walls.Contains(current) && visited.Add(current))
		{
			if (current.Neighbors().Any(c => players.TryGetValue(c, out var unit) && unit.isGoblin == isLookingForGoblin))
			{
				return (current, length);
			}
			else
			{
				foreach (Coord neighbor in current.Neighbors())
				{
					toVisit.Enqueue((neighbor, length + 1));
				}
			}
		}
	}
	return null;
}

for (int elfAttack = 4; true; elfAttack++)
{
	for (int rounds = 0; true; rounds++)
	{
		Coord[] playerCoords = players.Keys.OrderBy(static c => c.y).ThenBy(static c => c.x).ToArray();
		for (int i = 0; i < playerCoords.Length; i++)
		{
			Coord coord = playerCoords[i];
			if (players.TryGetValue(coord, out var unit))
			{
				IEnumerable<(Coord c, Unit o)> getNeighbors(Coord coord) =>
					coord.Neighbors()
						.Where(players.ContainsKey)
						.Select(c => (c, o: players[c]))
						.Where(p => p.o.isGoblin != unit.isGoblin);
				IEnumerable<(Coord c, Unit o)> neighbors = getNeighbors(coord);
				if (!neighbors.Any())
				{
					Coord moveTo = coord.Neighbors()
						.AsParallel()
						.Where(c => !players.ContainsKey(c) && !walls.Contains(c))
						.Select(c => (c, t: BreadthFirstSearch(c, !unit.isGoblin)))
						.Where(static p => p.t.HasValue)
						.OrderBy(static p => p.t?.d)
						.ThenBy(static p => p.t?.c.y)
						.ThenBy(static p => p.t?.c.x)
						.ThenBy(static p => p.c.y)
						.ThenBy(static p => p.c.x)
						.Cast<(Coord c, (Coord, int)?)?>()
						.FirstOrDefault()?.c;
					if (moveTo is Coord mt)
					{
						players.Remove(coord);
						players.Add(mt, unit);
						neighbors = getNeighbors(mt);
					}
				}
				if (neighbors.Any())
				{
					var (adj, other) = neighbors.OrderBy(static p => p.o.hp).ThenBy(static p => p.c.y).ThenBy(static p => p.c.x).First();
					players.Remove(adj);
					int hp = other.hp - (unit.isGoblin ? 3 : elfAttack);
					if (hp > 0)
					{
						players.Add(adj, other with { hp = hp });
					}
					else if (!other.isGoblin)
					{
						goto retry;
					}
					else if (players.Values.All(u => u.isGoblin != other.isGoblin))
					{
						if (i + 1 == playerCoords.Length)
						{
							rounds++;
						}
						WriteLine(rounds * players.Values.Sum(u => u.hp));
						return;
					}
				}
			}
		}
	}
	retry:
	players = new Dictionary<Coord, Unit>(startingPlayers);
}
