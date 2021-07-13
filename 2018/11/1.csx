static long PowerLevel(long x, long y, long serialNumber)
{
	long rackID = x + 10;
	return ((rackID * y + serialNumber) * rackID / 100L) % 10L - 5L;
}

const long Width = 300L;
const long Height = 300L;
const long SectionWidth = 3L;
const long SectionHeight = 3L;

long serialNumber = Int64.Parse(File.ReadAllText("input.txt").Trim());
(long x, long y, long powerLevel) max = (0, 0, 0);
for (long y = 1L; y <= Height - SectionHeight; y++)
{
	for (long x = 1L; x <= Width - SectionWidth; x++)
	{
		long sectionPower = 0;
		for (long dy = 0; dy < SectionHeight; dy++)
		{
			for (long dx = 0; dx < SectionWidth; dx++)
			{
				sectionPower += PowerLevel(x + dx, y + dy, serialNumber);
			}
		}
		if (sectionPower > max.powerLevel)
		{
			max = (x, y, sectionPower);
		}
	}
}
WriteLine($"{max.x},{max.y}");
