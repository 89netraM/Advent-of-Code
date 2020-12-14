/// <remarks>
/// With inspiration from https://github.com/mebeim/aoc/blob/master/2019/solutions/day12.py
/// </remarks>

#load ".\Vector.csx"
#load ".\Body.csx"
#load ".\Universe.csx"

long GCD(long a, long b)
{
	a = Math.Abs(a);
	b = Math.Abs(b);
	while (a != 0 && b != 0)
	{
		if (a > b)
		{
			a %= b;
		}
		else
		{
			b %= a;
		}
	}
	return a + b;
}
long LCM(long a, long b) => Math.Abs(a * b) / GCD(a, b);

Universe u = new Universe(
	File.ReadAllLines("input.txt")
		.Select(l => new Body(Vector.Parse(l)))
);

long? stepsX = null;
long? stepsY = null;
long? stepsZ = null;
long steps = 0;
while (!stepsX.HasValue || !stepsY.HasValue || !stepsZ.HasValue)
{
	u = u.Step();
	steps++;

	if (u.Bodies.All(b => b.Velocity.X == 0))
	{
		stepsX = steps;
	}
	if (u.Bodies.All(b => b.Velocity.Y == 0))
	{
		stepsY = steps;
	}
	if (u.Bodies.All(b => b.Velocity.Z == 0))
	{
		stepsZ = steps;
	}
}

WriteLine(LCM(LCM(stepsX.Value, stepsY.Value), stepsZ.Value) * 2);