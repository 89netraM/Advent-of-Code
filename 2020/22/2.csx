var result = File.ReadAllText("input.txt")
	.Split("\n\n")
	.Select(l => new LinkedList<long>(l.Split("\n").Skip(1).Select(Int64.Parse)))
	.ToArray();
var p1 = result[0];
var p2 = result[1];

bool PlayGame(LinkedList<long> a, LinkedList<long> b)
{
	IList<(IEnumerable<long> a, IEnumerable<long> b)> previousRounds = new List<(IEnumerable<long>, IEnumerable<long>)>();
	while (a.Count > 0 && b.Count > 0)
	{
		var p = (a: new long[a.Count], b: new long[b.Count]);
		a.CopyTo(p.a, 0);
		b.CopyTo(p.b, 0);
		if (!previousRounds.Any(pp => pp.a.SequenceEqual(p.a) && pp.b.SequenceEqual(p.b)))
		{
			previousRounds.Add(p);

			long aV = a.First.Value;
			long bV = b.First.Value;
			if (a.Count > aV && b.Count > bV)
			{
				if (PlayGame(new LinkedList<long>(a.Skip(1).Take((int)aV)), new LinkedList<long>(b.Skip(1).Take((int)bV))))
				{
					a.AddLast(a.First.Value);
					a.RemoveFirst();
					a.AddLast(b.First.Value);
					b.RemoveFirst();
				}
				else
				{
					b.AddLast(b.First.Value);
					b.RemoveFirst();
					b.AddLast(a.First.Value);
					a.RemoveFirst();
				}
			}
			else if (aV > bV)
			{
				a.AddLast(a.First.Value);
				a.RemoveFirst();
				a.AddLast(b.First.Value);
				b.RemoveFirst();
			}
			else
			{
				b.AddLast(b.First.Value);
				b.RemoveFirst();
				b.AddLast(a.First.Value);
				a.RemoveFirst();
			}
		}
		else
		{
			return true;
		}
	}
	return a.Count > 0;
}

var winner = PlayGame(p1, p2) ? p1 : p2;
WriteLine(
	winner.Select((l, i) => l * (winner.Count - i)).Sum()
);