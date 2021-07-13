static long PowerLevel(long x, long y, long serialNumber)
{
	long rackID = x + 10;
	return ((rackID * y + serialNumber) * rackID / 100L) % 10L - 5L;
}

const long Size = 300L;

long serialNumber = Int64.Parse(File.ReadAllText("input.txt").Trim());
(long x, long y, long size, long powerLevel) max = Enumerable.Range(1, (int)Size)
	.AsParallel()
	.Select(sectionSize =>
	{
		(long x, long y, long size, long powerLevel) max = (0, 0, 0, 0);
		for (long y = 1L; y <= Size - sectionSize; y++)
		{
			for (long x = 1L; x <= Size - sectionSize; x++)
			{
				long sectionPower = 0;
				for (long dy = 0; dy < sectionSize; dy++)
				{
					for (long dx = 0; dx < sectionSize; dx++)
					{
						sectionPower += PowerLevel(x + dx, y + dy, serialNumber);
					}
				}
				if (sectionPower > max.powerLevel)
				{
					max = (x, y, sectionSize, sectionPower);
				}
			}
		}
		return max;
	})
	.Aggregate(static (a, c) => a.powerLevel > c.powerLevel ? a : c);
WriteLine($"{max.x},{max.y},{max.size}");
