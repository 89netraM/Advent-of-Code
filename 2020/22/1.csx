var result = File.ReadAllText("input.txt")
	.Split("\n\n")
	.Select(l => new LinkedList<long>(l.Split("\n").Skip(1).Select(Int64.Parse)))
	.ToArray();
var p1 = result[0];
var p2 = result[1];

while (p1.Count > 0 && p2.Count > 0)
{
	if (p1.First.Value > p2.First.Value)
	{
		p1.AddLast(p1.First.Value);
		p1.RemoveFirst();
		p1.AddLast(p2.First.Value);
		p2.RemoveFirst();
	}
	else
	{
		p2.AddLast(p2.First.Value);
		p2.RemoveFirst();
		p2.AddLast(p1.First.Value);
		p1.RemoveFirst();
	}
}

var winner = p1.Count > 0 ? p1 : p2;
WriteLine(
	winner.Select((l, i) => l * (winner.Count - i)).Sum()
);