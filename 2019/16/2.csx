/// <remarks>
/// What's happening here?
/// Line 23 and below is straight up stolen from https://gist.github.com/Aneurysm9/5912dc65265068a6c9fa799f88697349
/// </remarks>

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

string input = File.ReadAllText("input.txt");
int offset = Int32.Parse(input.Substring(0, 7));
long[] number = Enumerable.Repeat(input.Select(c => (long)Char.GetNumericValue(c)), 10000).SelectMany(x => x).ToArray();
long[] output = new long[number.Length];
long tmp = 0;
for (int i = 0; i < 100; i++)
{
	tmp = 0;
	for (int j = number.Length - 1; j >= offset; j--)
	{
		tmp = (tmp + number[j]) % 10;
		output[j] = tmp;
	}
	(number, output) = (output, number);
}

WriteLine(String.Concat(number.Skip(offset).Take(8)));