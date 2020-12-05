int Map(string line)
{
	return Binary(line[0..7], 'F', 'B', 128) * 8 + Binary(line[7..10], 'L', 'R', 8);
}

int Binary(string s, char low, char high, int max)
{
	int min = 0;
	foreach (char c in s)
	{
		if (c == low)
		{
			max = (max - min) / 2 + min;
		}
		else if (c == high)
		{
			min = (max - min) / 2 + min;
		}
	}
	return min;
}

var items = File.ReadAllLines("input.txt").Select(Map).OrderBy(i => i).ToArray();
int prev = items[0];
foreach (var item in items[1..])
{
	if (prev + 1 != item)
	{
		WriteLine(prev + 1);
		break;
	}
	prev = item;
}