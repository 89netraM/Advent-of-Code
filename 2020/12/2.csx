(int, int) Rotate((int x, int y) w, int deg)
{
	double rad = deg / 180.0 * Math.PI;
	return (
		w.x * (int)Math.Cos(rad) - w.y * (int)Math.Sin(rad),
		w.x * (int)Math.Sin(rad) + w.y * (int)Math.Cos(rad)
	);
}

var result = File.ReadAllText("input.txt")
	.Split("\n")
	.Select(x => (d: x[0], i: Int32.Parse(x[1..])))
	.Aggregate((p: (x: 0, y: 0), w: (x: 10, y: -1)), (a, x) => x.d switch
	{
		'N' => (a.p, (a.w.x, a.w.y - x.i)),
		'S' => (a.p, (a.w.x, a.w.y + x.i)),
		'E' => (a.p, (a.w.x + x.i, a.w.y)),
		'W' => (a.p, (a.w.x - x.i, a.w.y)),
		'L' => (a.p, Rotate(a.w, -x.i)),
		'R' => (a.p, Rotate(a.w, x.i)),
		'F' => ((a.p.x + x.i * a.w.x, a.p.y + x.i * a.w.y), a.w)
	});

WriteLine(
	Math.Abs(result.p.x) + Math.Abs(result.p.y)
);