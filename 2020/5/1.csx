int Map(string line)
{
	return Binary(line[0..7], 'F', 'B', 128) * 8 + Binary(line[7..10], 'L', 'R', 8);
}
int Fold(int all, int i)
{
	return i > all ? i : all;
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

WriteLine(
	File.ReadAllLines("input.txt")
		.Select(Map)
		.Aggregate(0, Fold)
);