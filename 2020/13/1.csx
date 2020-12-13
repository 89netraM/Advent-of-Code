var lines = File.ReadAllText("input.txt")
	.Split("\n");
var timestamp = Int32.Parse(lines[0]);

(int, int) GDC(int a)
{
	return (a, (timestamp - (timestamp % a)) + a);
}
var result = lines[1]
	.Split(",")
	.Where(x => x != "x")
	.Select(Int32.Parse)
	.Select(GDC)
	.Aggregate((0, Int32.MaxValue), (a, x) => x.Item2 < a.Item2 ? x : a);

WriteLine(
	result.Item1 * (result.Item2 - timestamp)
);