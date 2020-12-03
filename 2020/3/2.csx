bool[][] map = File.ReadAllLines("input.txt").Select(l => l.Select(c => c == '#').ToArray()).ToArray();

long CheckSlope((long dx, long dy) d)
{
	long trees = 0;
	long x = 0;
	for (long y = 0; y < map.Length; y += d.dy)
	{
		if (map[(int)y][(int)x])
		{
			trees++;
		}
		x = (x + d.dx) % map[(int)y].Length;
	}
	return trees;
}

WriteLine(
	new (long, long)[] { (1, 1), (3, 1), (5, 1), (7, 1), (1, 2) }
		.Select(CheckSlope)
		.Aggregate(1L, (a, b) => a * b)
);