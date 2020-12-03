bool[] numbers = new bool[2020];
foreach (int n in File.ReadAllLines("./input.txt").Select(Int32.Parse))
{
	if (n < 2020)
	{
		numbers[n] = true;
		if (numbers[2020 - n])
		{
			WriteLine(n * (2020 - n));
			break;
		}
	}
}