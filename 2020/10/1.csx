(int, int, int) Fold((int a, int b, int p) all, int i)
{
	int diff = i - all.p;
	if (diff == 1)
	{
		return (all.a + 1, all.b, i);
	}
	else if (diff == 3)
	{
		return (all.a, all.b + 1, i);
	}
	else
	{
		return (all.a, all.b, i);
	}
}

(int a, int b, int p) result = File.ReadAllLines("input.txt")
	.Select(Int32.Parse)
	.Append(0)
	.OrderBy(x => x)
	.Aggregate((0, 1, 0), Fold);
WriteLine(
	result.a * result.b
);