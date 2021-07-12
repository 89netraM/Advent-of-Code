HashSet<long> seen = new HashSet<long> { 0 };
long[] changes = File.ReadAllLines("input.txt").Select(Int64.Parse).ToArray();
long sum = 0;
while (true)
{
	foreach (long change in changes)
	{
		sum += change;
		if (!seen.Add(sum))
		{
			WriteLine(sum);
			return;
		}
	}
}
