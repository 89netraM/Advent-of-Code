int[] numbers = File.ReadAllLines("./input.txt").Select(Int32.Parse).ToArray();

for (int i = 0; i < numbers.Length; i++)
{
	for (int j = i + 1; j < numbers.Length; j++)
	{
		if (numbers[i] + numbers[j] == 2020) {
			WriteLine($"{numbers[i]} + {numbers[j]} = 2020; {numbers[i]} * {numbers[j]} = {numbers[i] * numbers[j]}");
		}
	}
}