bool[][] map = File.ReadAllLines("input.txt").Select(l => l.Select(c => c == '#').ToArray()).ToArray();

int trees = 0;
int x = 0;
for (int y = 0; y < map.Length; y++)
{
	if (map[y][x])
	{
		trees++;
	}
	x = (x + 3) % map[y].Length;
}
WriteLine(trees);