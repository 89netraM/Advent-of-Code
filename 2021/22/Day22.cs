using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2021
{
	[Day(22)]
	public class Day22
	{
		[Part(1)]
		public object Part1(string input)
		{
			var cubes = new HashSet<Vector3>();
			foreach (var ((onOff, from), to) in input.Lines().Extract<(string, Vector3)>(@"(on|off) (x=(-?\d+)..-?\d+,y=(-?\d+)..-?\d+,z=(-?\d+)..-?\d+)").Zip(input.Lines().Extract<Vector3>(@"x=-?\d+..(-?\d+),y=-?\d+..(-?\d+),z=-?\d+..(-?\d+)")))
			{
				var on = onOff == "on";
				for (long z = Math.Max(from.Z, -50L); z <= Math.Min(to.Z, 50L); z++)
					for (long y = Math.Max(from.Y, -50L); y <= Math.Min(to.Y, 50L); y++)
						for (long x = Math.Max(from.X, -50L); x <= Math.Min(to.X, 50L); x++)
							if (on)
								cubes.Add(new Vector3(x, y, z));
							else
								cubes.Remove(new Vector3(x, y, z));
			}
			return cubes.Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var regions = new List<Region>();
			var regions2 = new List<Region>();
			foreach (var (on, region) in input.Lines()
				.Extract<(string, (long, long), (long, long), (long, long))>(@"(on|off) (x=(-?\d+)..(-?\d+)),(y=(-?\d+)..(-?\d+)),(z=(-?\d+)..(-?\d+))")
				.Select(static t => (t.Item1 == "on", new Region(t.Item2, t.Item3, t.Item4))))
			{
				foreach (var other in regions)
				{
					regions2.AddRange(other.Without(region));
				}
				if (on)
				{
					regions2.Add(region);
				}
				(regions, regions2) = (regions2, regions);
				regions2.Clear();
			}

			return regions.Sum(static r => r.Volume);
		}

		readonly struct Region
		{
			public Vector3 From { get; }
			public Vector3 To { get; }
			
			public long Volume =>
				(To.X - From.X + 1) *
				(To.Y - From.Y + 1) *
				(To.Z - From.Z + 1);

			public Region(Vector3 from, Vector3 to) =>
				(From, To) = (from, to);
			public Region((long f, long t) x, (long f, long t) y, (long f, long t) z) =>
				(From, To) = (new Vector3(x.f, y.f, z.f), new Vector3(x.t, y.t, z.t));

			public Region? Intersect(Region other)
			{
				var from = From.MaxParts(other.From);
				var to = To.MinParts(other.To);
				if (from.X <= to.X && from.Y <= to.Y && from.Z <= to.Z)
				{
					return new Region(from, to);
				}
				else
				{
					return null;
				}
			}

			public IEnumerable<Region> Without(Region other)
			{
				if (Intersect(other) is Region intersection)
				{
					if (intersection.From.X > From.X)
					{
						yield return new Region(new Vector3(From.X, From.Y, From.Z), new Vector3(intersection.From.X - 1, To.Y, To.Z));
					}
					if (intersection.To.X < To.X)
					{
						yield return new Region(new Vector3(intersection.To.X + 1, From.Y, From.Z), new Vector3(To.X, To.Y, To.Z));
					}
					if (intersection.From.Y > From.Y)
					{
						yield return new Region(new Vector3(intersection.From.X, From.Y, From.Z), new Vector3(intersection.To.X, intersection.From.Y - 1, To.Z));
					}
					if (intersection.To.Y < To.Y)
					{
						yield return new Region(new Vector3(intersection.From.X, intersection.To.Y + 1, From.Z), new Vector3(intersection.To.X, To.Y, To.Z));
					}
					if (intersection.From.Z > From.Z)
					{
						yield return new Region(new Vector3(intersection.From.X, intersection.From.Y, From.Z), new Vector3(intersection.To.X, intersection.To.Y, intersection.From.Z - 1));
					}
					if (intersection.To.Z < To.Z)
					{
						yield return new Region(new Vector3(intersection.From.X, intersection.From.Y, intersection.To.Z + 1), new Vector3(intersection.To.X, intersection.To.Y, To.Z));
					}
				}
				else
				{
					yield return this;
				}
			}
		}
	}
}
