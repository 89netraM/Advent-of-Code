Dictionary<int, int> spoken = new Dictionary<int, int>();
int[] starting = File.ReadAllText("input.txt")
	.Split(',')
	.Select(Int32.Parse)
	.ToArray();
for (int i = 0; i < starting.Length; i++)
{
	spoken[starting[i]] = i;
}

int next = 0;
for (int i = starting.Length; i < 30000000 - 1; i++)
{
	int nextNext = spoken.ContainsKey(next) ? i - spoken[next] : 0;
	spoken[next] = i;
	next = nextNext;
}

WriteLine(next);