bool CanSum(long[] previous, long num)
{
	for (int i = 0; i < previous.Length; i++)
	{
		for (int j = i + 1; j < previous.Length; j++)
		{
			if (previous[i] + previous[j] == num)
			{
				return true;
			}
		}
	}
	return false;
}

long[] numbers = File.ReadAllLines("input.txt")
		.Select(Int64.Parse)
		.ToArray();
long FirstInvlid(long[] numbers, int pre)
{
	for (int i = pre; i < numbers.Length; i++)
	{
		if (!CanSum(numbers[(i - pre)..i], numbers[i]))
		{
			return numbers[i];
		}
	}
	return -1;
}
long firstInvalid = FirstInvlid(numbers, 25);

for (int i = 0; i < numbers.Length; i++)
{
	for (int j = i + 2; j < numbers.Length; j++)
	{
		if (numbers[i..j].Sum() == firstInvalid)
		{
			WriteLine(numbers[i..j].Min() + numbers[i..j].Max());
			break;
		}
	}
}