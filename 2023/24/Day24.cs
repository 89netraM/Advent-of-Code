using System;
using System.Linq;
using AoC.Library;
using RegExtract;
using Microsoft.Z3;

namespace AoC.Year2023;

[Day(24)]
public class Day24
{
	[Part(1)]
	public object Part1(string input)
	{
		var min = new Vector2(200000000000000, 200000000000000);
		var max = new Vector2(400000000000000, 400000000000000);

		var hails = input.Lines()
			.Extract<Hail>(@"(((-?\d+),\s*(-?\d+),\s*(-?\d+))\s*@\s*((-?\d+),\s*(-?\d+),\s*(-?\d+)))")
			.ToArray();

		long intersections = 0;
		for (int i = 0; i < hails.Length - 1; i++)
		{
			for (int j = i + 1; j < hails.Length; j++)
			{
				if (IntersectXY(hails[i], hails[j]) is Vector2 cross && IsWithin(min, max, cross))
				{
					intersections++;
				}
			}
		}
		return intersections;
	}

	private static Vector2? IntersectXY(Hail a, Hail b)
	{
		if (b.Velocity.X == 0) { return null; }

		var nämnare = b.Velocity.X * a.Velocity.Y - a.Velocity.X * b.Velocity.Y;

		if (nämnare == 0) { return null; }

		var t1 = (-b.Velocity.X * a.Position.Y + b.Velocity.X * b.Position.Y + b.Velocity.Y * a.Position.X - b.Velocity.Y * b.Position.X) / nämnare;
		var t2 = (-a.Velocity.X * a.Position.Y + a.Velocity.X * b.Position.Y + a.Velocity.Y * a.Position.X - a.Velocity.Y * b.Position.X) / nämnare;

		if (t1 < 0 || t2 < 0) { return null; }

		return a.Position.XY + a.Velocity.XY * t1;
	}

	private static bool IsWithin(Vector2 min, Vector2 max, Vector2 point) =>
		min.X <= point.X && point.X <= max.X
			&& min.Y <= point.Y && point.Y <= max.Y;

	[Part(2)]
	public object Part2(string input)
	{
		var hails = input.Lines().Extract<Hail>(@"(((-?\d+),\s*(-?\d+),\s*(-?\d+))\s*@\s*((-?\d+),\s*(-?\d+),\s*(-?\d+)))");

		using var ctx = new Context();
		var solver = ctx.MkSolver();

		var rockPos = (x: ctx.MkRealConst("rockPosX"), y: ctx.MkRealConst("rockPosY"), z: ctx.MkRealConst("rockPosZ"));
		var rockVel = (x: ctx.MkRealConst("rockVelX"), y: ctx.MkRealConst("rockVelY"), z: ctx.MkRealConst("rockVelZ"));
		foreach (var (i, hail) in hails.Enumerate())
		{
			var t = ctx.MkRealConst($"hail{i}t");
			solver.Assert(t > 0);
			solver.Assert(
				ctx.MkEq(hail.Position.X + hail.Velocity.X * t, rockPos.x + rockVel.x * t),
				ctx.MkEq(hail.Position.Y + hail.Velocity.Y * t, rockPos.y + rockVel.y * t),
				ctx.MkEq(hail.Position.Z + hail.Velocity.Z * t, rockPos.z + rockVel.z * t));
		}

		if (solver.Check() != Status.SATISFIABLE)
		{
			throw new Exception("Unsatisfiable");
		}

		return solver.Model.Eval(rockPos.x + rockPos.y + rockPos.z);
	}

	private record Hail(Vector3 Position, Vector3 Velocity);
}
