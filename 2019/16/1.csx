long[] pattern = new long[] { 0, 1, 0, -1 };
IEnumerable<long> Pattern(long reps)
{
	while (true)
	{
		foreach (long l in pattern)
		{
			for (long a = 0; a <= reps; a++)
			{
				yield return l;
			}
		}
	}
}

IList<long> input = File.ReadAllText("input.txt").Select(c => (long)Char.GetNumericValue(c)).ToList();

for (int i = 0; i < 100; i++)
{
	input = input.Select((_, i) => Math.Abs(input.Zip(Pattern(i).Skip(1), (a, b) => a * b).Sum() % 10)).ToList();
}

WriteLine(String.Concat(input.Take(8)));