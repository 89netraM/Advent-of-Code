record Rule(bool m2, bool m1, bool c, bool p1, bool p2, bool n);
static bool MatchRule(this Rule r, bool m2, bool m1, bool c, bool p1, bool p2) =>
	m2 == r.m2 && m1 == r.m1 && c == r.c && p1 == r.p1 && p2 == r.p2;

string[] input = File.ReadAllLines("input.txt");

Rule[] rules = input.Skip(2)
	.Select(static l => new Rule(l[0] == '#', l[1] == '#', l[2] == '#', l[3] == '#', l[4] == '#', l[9] == '#'))
	.ToArray();

HashSet<long> current = new HashSet<long>(input[0].Select(static (c, i) => (c, i)).Where(static p => p.c == '#').Select(static p => (long)p.i - 15L));
HashSet<long> next = new HashSet<long>();
const long Generations = 150;
long prev = current.Sum();
WriteLine($"0\t{prev}");
for (long g = 0; g < Generations; g++)
{
	long min = current.Min() - 2;
	long max = current.Max() + 2;
	for (long i = min; i <= max; i++)
	{
		if (rules.FirstOrDefault(r => r.MatchRule(current.Contains(i - 2), current.Contains(i - 1), current.Contains(i), current.Contains(i + 1), current.Contains(i + 2))) is Rule r && r.n)
		{
			next.Add(i);
		}
	}

	long sum = next.Sum();
	WriteLine($"{g}\t{sum}\t{sum - prev}");
	prev = sum;

	(current, next) = (next, current);
	next.Clear();
}

// Notice that the difference is constantly +62 after generation 99 (5629), and
// calculate `5629 + (50000000000 - 99) * 62` for the answer.
