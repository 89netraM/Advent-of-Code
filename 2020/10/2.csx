var js = File.ReadAllLines("input.txt")
	.Select(Int32.Parse)
	.Append(0)
	.OrderByDescending(x => x)
	.ToArray();
var options = new long[js.Length];
options[0] = 1;

for (int i = 0; i < js.Length; i++)
{
	int prev = js[i];
	int min = prev - 3;
	int max = prev - 1;

	for (int j = 1; i + j < js.Length && min <= js[i + j] && js[i + j] <= max; j++)
	{
		options[i + j]++;
	}
}
for (int i = 1; i < options.Length; i++)
{
	long c = options[i];
	options[i] = 0;
	for (long j = 1; j <= c; j++)
	{
		options[i] += options[i - j];
	}
}
WriteLine(options[^1]);