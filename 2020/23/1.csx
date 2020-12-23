var cups = File.ReadAllText("input.txt")
	.Select(c => Int64.Parse(c.ToString()))
	.ToList();

var current = cups[^1];
var pickUp = new long[3];
long next = -1;

for (int i = 0; i < 100; i++)
{
	var nextIndex = (cups.IndexOf(current) + 1) % cups.Count;
	current = cups[nextIndex];
	next = current - 1;
	nextIndex = (nextIndex + 1) % cups.Count;
	for (int j = 0; j < pickUp.Length; j++)
	{
		pickUp[j] = cups[nextIndex];
		cups.RemoveAt(nextIndex);
		if (nextIndex >= cups.Count)
		{
			nextIndex = 0;
		}
	}

	while (!cups.Contains(next))
	{
		next = next <= 1 ? cups.Count + pickUp.Length : next - 1;
	}
	var index = cups.IndexOf(next) + 1;
	cups.InsertRange(index, pickUp);
}

WriteLine(
	String.Join(
		"",
		cups.SkipWhile(l => l != 1).Skip(1).Concat(cups.TakeWhile(l => l != 1))
	)
);