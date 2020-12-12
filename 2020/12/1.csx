var result = File.ReadAllText("input.txt")
	.Split("\n")
	.Select(x => (d: x[0], i: Int32.Parse(x[1..])))
	.Aggregate((p: (x: 0, y: 0), d: 0), (a, x) => x.d switch {
		'N' => ((a.p.x, a.p.y - x.i), a.d),
		'S' => ((a.p.x, a.p.y + x.i), a.d),
		'E' => ((a.p.x + x.i, a.p.y), a.d),
		'W' => ((a.p.x - x.i, a.p.y), a.d),
		'L' => ((a.p.x, a.p.y), a.d - x.i),
		'R' => ((a.p.x, a.p.y), a.d + x.i),
		'F' => ((a.p.x + x.i * (int)Math.Cos(a.d / 180.0 * Math.PI), a.p.y + x.i * (int)Math.Sin(a.d / 180.0 * Math.PI)), a.d)
	});

WriteLine(
	Math.Abs(result.p.x) + Math.Abs(result.p.y)
);