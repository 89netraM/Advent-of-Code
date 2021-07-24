record Coord(int x, int y, int z, int t);
static int ManhattanDistance(this Coord a, Coord b) =>
	Math.Abs(a.x - b.x) + Math.Abs(a.y - b.y) + Math.Abs(a.z - b.z) + Math.Abs(a.t - b.t);
static Coord ParseCoord(string coord)
{
	string[] split = coord.Split(',');
	return new Coord(Int32.Parse(split[0]), Int32.Parse(split[1]), Int32.Parse(split[2]), Int32.Parse(split[3]));
}

List<HashSet<Coord>> constellations = File.ReadLines("input.txt").Select(ParseCoord).Select(static c => new HashSet<Coord> { c }).ToList();
bool didChange = true;
while (didChange)
{
	didChange = false;
	for (int i = 0; i < constellations.Count; i++)
	{
		HashSet<Coord> constellation = constellations[i];
		for (int j = i + 1; j < constellations.Count; j++)
		{
			HashSet<Coord> other = constellations[j];
			if (constellation.Any(c => other.Any(o => ManhattanDistance(c, o) <= 3)))
			{
				constellation.UnionWith(other);
				constellations.RemoveAt(j);
				j--;
				didChange = true;
			}
		}
	}
}

WriteLine(constellations.Count);
