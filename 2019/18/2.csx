using System.Collections.Immutable;

const char ENTRANCE = '@';
const char WALL = '#';

record Coord(int x, int y);
record Key(
	Coord l,
	Dictionary<char, Link> links
);

abstract class Link
{
	public int Distance { get; protected init; }
	public abstract bool AllClear(IEnumerable<char> openDoors);
}
class Reachable : Link
{
	public ImmutableList<char> Doors { get; }

	public Reachable(int distance, ImmutableList<char> doors) =>
		(Distance, Doors) = (distance, doors);

	public override bool AllClear(IEnumerable<char> openDoors) =>
		Doors.All(d => openDoors.Contains(d));
}
class NotReachable : Link
{
	public NotReachable() =>
		(Distance) = (Int32.MaxValue);

	public override bool AllClear(IEnumerable<char> openDoors) => false;
}

Key BuildKey(Coord l) => new(l, new());
bool IsKey(char c) => 'a' <= c && c <= 'z';
bool IsDoor(char c) => 'A' <= c && c <= 'Z';

List<List<char>> UpdateInput(IReadOnlyList<string> inputLines)
{
	List<List<char>> v = new();
	Coord start = new(0, 0);
	for (int y = 0; y < inputLines.Count; y++)
	{
		v.Add(inputLines[y].ToList());
		for (int x = 0; x < inputLines[y].Length; x++)
		{
			if (inputLines[y][x] == ENTRANCE)
			{
				start = new(x, y);
			}
		}
	}
	v[start.y - 1][start.x - 1] = ENTRANCE;
	v[start.y - 1][start.x + 0] = WALL;
	v[start.y - 1][start.x + 1] = ENTRANCE;
	v[start.y + 0][start.x - 1] = WALL;
	v[start.y + 0][start.x + 0] = WALL;
	v[start.y + 0][start.x + 1] = WALL;
	v[start.y + 1][start.x - 1] = ENTRANCE;
	v[start.y + 1][start.x + 0] = WALL;
	v[start.y + 1][start.x + 1] = ENTRANCE;
	return v;
}

record ParseTuple(
	List<List<char>> v,
	ImmutableList<char> entrances,
	Dictionary<char, Key> keys
);
ParseTuple ParseLine(ParseTuple pt, (List<char>, int) c)
{
	var (line, y) = c;
	ImmutableList<char> entrances = pt.entrances;
	for (int x = 0; x < line.Count; x++)
	{
		if (IsKey(line[x]))
		{
			pt.keys[line[x]] = BuildKey(new(x, y));
		}
		else if (line[x] == ENTRANCE)
		{
			char entrance = (char)(entrances.Count + 48);
			pt.keys[entrance] = BuildKey(new(x, y));
			entrances = entrances.Add(entrance);
		}
	}
	pt.v.Add(line);
	return new(pt.v, entrances, pt.keys);
}
ParseTuple Parse(List<List<char>> v) =>
	v.Select(static (l, i) => (l, i))
		.Aggregate(new ParseTuple(new(), ImmutableList.Create<char>(), new()), ParseLine);

Coord[] dirs = new Coord[] { new(0, -1), new(0, 1), new(-1, 0), new(1, 0) };
IEnumerable<Coord> GetNext(Coord c) =>
	dirs.Select(d => new Coord(c.x + d.x, c.y + d.y));

bool NotWall(List<List<char>> v, Coord c) =>
	0 <= c.y && c.y < v.Count &&
		0 <= c.x && c.x < v[c.y].Count &&
		v[c.y][c.x] != WALL;

Link GetLink(List<List<char>> v, Coord from, Coord to)
{
	Queue<(Coord, int, ImmutableList<char>)> queue = new();
	queue.Enqueue((from, 0, ImmutableList.Create<char>()));
	HashSet<Coord> visited = new();
	while (queue.Count > 0)
	{
		var (coord, distance, doors) = queue.Dequeue();
		visited.Add(coord);
		foreach (Coord next in GetNext(coord))
		{
			if (!visited.Contains(next) && NotWall(v, coord))
			{
				if (next == to)
				{
					return new Reachable(distance + 1, doors);
				}
				else
				{
					char c = v[next.y][next.x];
					queue.Enqueue((
						next,
						distance + 1,
						IsDoor(c) ? doors.Add(Char.ToLower(c)) : doors
					));
				}
			}
		}
	}
	return new NotReachable();
}

int FindShortestPath(ImmutableList<char> names, IReadOnlyDictionary<char, Key> keys, ImmutableList<char> visited, ImmutableList<char> positions, int distance, Dictionary<string, int> memo)
{
	if (names.Count == visited.Count)
	{
		return distance;
	}
	else
	{
		IEnumerable<char> remaining = names.Where(k => !visited.Contains(k));
		string mk = $"{String.Concat(positions)}:{String.Concat(remaining)}";
		int min;
		if (!memo.TryGetValue(mk, out min))
		{
			min = Int32.MaxValue;
			for (int pi = 0; pi < positions.Count; pi++)
			{
				char current = positions[pi];
				Dictionary<char, Link> links = keys[current].links;
				foreach (char nk in remaining)
				{
					if (links[nk].AllClear(visited))
					{
						min = Math.Min(
							min,
							FindShortestPath(names, keys, visited.Add(nk), positions.SetItem(pi, nk), distance + links[nk].Distance, memo)
						);
					}
				}
			}
			min -= distance;
			memo[mk] = min;
		}
		return min + distance;
	}
}

var (v, entrances, keys) = Parse(UpdateInput(File.ReadAllLines("input.txt")));
ImmutableList<char> names = keys.Keys.OrderBy(static c => c).ToImmutableList();
int distance = 0;
ImmutableList<char> doors;
for (int i = 0; i < names.Count; i++)
{
	char aName = names[i];
	Key a = keys[aName];
	for (int j = i + 1; j < names.Count; j++)
	{
		char bName = names[j];
		Key b = keys[bName];
		Link link = GetLink(v, a.l, b.l);
		a.links[bName] = link;
		b.links[aName] = link;
	}
}
distance = FindShortestPath(names, keys, entrances, entrances, 0, new());

WriteLine(distance);
