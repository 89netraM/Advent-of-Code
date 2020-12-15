int[] spoken = new int[2020];
int[] starting = File.ReadAllText("input.txt")
	.Split(',')
	.Select(Int32.Parse)
	.ToArray();
starting.CopyTo(spoken, 0);

int DistanceTo(int i, int t)
{
	for (int d = i - 1; d >= 0; d--)
	{
		if (spoken[d] == t)
		{
			return i - d;
		}
	}
	return -1;
}

for (int i = starting.Length - 1; i < spoken.Length - 1; i++)
{
	int d = DistanceTo(i, spoken[i]);
	if (d == -1)
	{
		spoken[i + 1] = 0;
	}
	else
	{
		spoken[i + 1] = d;
	}
}

WriteLine(spoken[2019]);