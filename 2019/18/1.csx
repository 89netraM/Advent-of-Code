using System.Collections.Immutable;

const char ENTRANCE = '@';
const char WALL = '#';

record Coord(int x, int y);
record Key(
	Coord l,
	Dictionary<char, int> distanceMap,
	Dictionary<char, ImmutableList<char>> doorMap
);

Key BuildKey(Coord l) => new(l, new(), new());
bool IsKey(char c) => 'a' <= c && c <= 'z';
bool IsDoor(char c) => 'A' <= c && c <= 'Z';

record ParseTuple(
	List<List<char>> v,
	Coord l,
	Dictionary<char, Key> keys
);
ParseTuple ParseLine(ParseTuple pt, (string, int) c)
{
	var (line, y) = c;
	var l = pt.l;
	for (int x = 0; x < line.Length; x++)
	{
		if (IsKey(line[x]))
		{
			pt.keys[line[x]] = BuildKey(new(x, y));
		}
		else if (line[x] == ENTRANCE)
		{
			l = new(x, y);
		}
	}
	pt.v.Add(line.ToList());
	return new(pt.v, l, pt.keys);
}
ParseTuple Parse(IEnumerable<string> inputLines) =>
	inputLines
		.Select(static (l, i) => (l, i))
		.Aggregate(new ParseTuple(new(), new(0, 0), new()), ParseLine);

Coord[] dirs = new Coord[] { new(0, -1), new(0, 1), new(-1, 0), new(1, 0) };
IEnumerable<Coord> GetNext(Coord c) =>
	dirs.Select(d => new Coord(c.x + d.x, c.y + d.y));

bool NotWall(List<List<char>> v, Coord c) =>
	0 <= c.y && c.y < v.Count &&
		0 <= c.x && c.x < v[c.y].Count &&
		v[c.y][c.x] != WALL;

(int distance, ImmutableList<char>) FindDistance(List<List<char>> v, Coord from, Coord to, char fromKey, char toKey)
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
					return (distance + 1, doors);
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
	throw new Exception($"Should find {toKey} from {fromKey}");
}

int FindShortestPath(ImmutableList<char> names, IReadOnlyDictionary<char, Key> keys, ImmutableList<char> visited, int distance, Dictionary<string, int> memo)
{
	if (names.Count == visited.Count - 1)
	{
		return distance;
	}
	else
	{
		char current = visited[visited.Count - 1];
		var keysToVisit = names.Where(c => !visited.Contains(c) && keys[c].doorMap[current].All(d => visited.Contains(d)));
		if (keysToVisit.Any())
		{
			string memoKey = $"{current}:{String.Concat(names.Where(k => !visited.Contains(k)).OrderBy(static k => k))}";
			int min;
			if (!memo.TryGetValue(memoKey, out min))
			{
				min = keysToVisit.Min(c => FindShortestPath(names, keys, visited.Add(c), distance + keys[c].distanceMap[current], memo)) - distance;
				memo[memoKey] = min;
			}
			return min + distance;
		}
		else
		{
			return distance;
		}
	}
}

var (v, start, keys) = Parse(File.ReadLines("input.txt"));
ImmutableList<char> names = keys.Keys.OrderBy(static c => c).ToImmutableList();
int distance = 0;
ImmutableList<char> doors;
for (int i = 0; i < names.Count; i++)
{
	char aName = names[i];
	Key a = keys[aName];
	(distance, doors) = FindDistance(v, start, a.l, ENTRANCE, aName);
	a.distanceMap[ENTRANCE] = distance;
	a.doorMap[ENTRANCE] = doors;
	for (int j = i + 1; j < names.Count; j++)
	{
		char bName = names[j];
		Key b = keys[bName];
		(distance, doors) = FindDistance(v, a.l, b.l, aName, bName);
		a.distanceMap[bName] = distance;
		b.distanceMap[aName] = distance;
		a.doorMap[bName] = doors;
		b.doorMap[aName] = doors;
	}
}
distance = FindShortestPath(names, keys, ImmutableList.Create(ENTRANCE), 0, new());

WriteLine(distance);
