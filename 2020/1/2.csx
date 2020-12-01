int[] numbers = File.ReadAllLines("./input.txt").Select(Int32.Parse).ToArray();

for (int i = 0; i < numbers.Length; i++)
{
	for (int j = i + 1; j < numbers.Length; j++)
	{
		for (int k = j + 1; k < numbers.Length; k++)
		{
			if (numbers[i] + numbers[j] + numbers[k] == 2020) {
				WriteLine($"{numbers[i]} + {numbers[j]} + {numbers[k]} = 2020; {numbers[i]} * {numbers[j]} * {numbers[k]} = {numbers[i] * numbers[j] * numbers[k]}");
			}
		}
	}
}