using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2021
{
	[Day(19)]
	public class Day19
	{
		[Part(1)]
		public object Part1(string input)
		{
			var scanners = input.Split("\n\n").Select(static i => i.Lines().Skip(1).Extract<Vector3>(@"(-?\d+),(-?\d+),(-?\d+)").ToHashSet()).ToArray();
			var scannerDiff = new Dictionary<(int from, int to), (Vector3 offset, int rotation)>();

			var diffCounter = new Dictionary<Vector3, long>();
			for (int i = 0; i < scanners.Length; i++)
			{
				for (int j = 0; j < scanners.Length; j++)
				{
					if (i == j) continue;
					foreach (var (rotation, scanner2) in AllRotations(scanners[j]))
					{
						diffCounter.Clear();
						foreach (var p1 in scanners[i])
						{
							foreach (var p2 in scanner2)
							{
								diffCounter.Increase(p1 - p2);
							}
						}

						var best = diffCounter.FirstOrDefault(static kvp => kvp.Value >= 12);
						if (best.Value >= 12)
						{
							scannerDiff[(j, i)] = (best.Key, rotation);
							break;
						}
					}
				}
			}

			for (int c = 0; c < 100; c++)
			{
				for (int i = 1; i < scanners.Length; i++)
				{
					for (int j = 0; j < scanners.Length; j++)
					{
						if (scannerDiff.TryGetValue((i, j), out var diff))
						{
							foreach (var p in NumberedRotation(scanners[i], diff.rotation))
							{
								scanners[j].Add(p + diff.offset);
							}
						}
					}
				}
			}

			return scanners[0].Count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var scanners = input.Split("\n\n").Select(static i => i.Lines().Skip(1).Extract<Vector3>(@"(-?\d+),(-?\d+),(-?\d+)").ToHashSet()).ToArray();
			var scannerDiff = new Dictionary<(int from, int to), (Vector3 offset, int rotation)>();

			var diffCounter = new Dictionary<Vector3, long>();
			for (int i = 0; i < scanners.Length; i++)
			{
				for (int j = 0; j < scanners.Length; j++)
				{
					if (i == j) continue;
					foreach (var (rotation, scanner2) in AllRotations(scanners[j]))
					{
						diffCounter.Clear();
						foreach (var p1 in scanners[i])
						{
							foreach (var p2 in scanner2)
							{
								diffCounter.Increase(p1 - p2);
							}
						}

						var best = diffCounter.FirstOrDefault(static kvp => kvp.Value >= 12);
						if (best.Value >= 12)
						{
							scannerDiff[(j, i)] = (best.Key, rotation);
							break;
						}
					}
				}
			}

			for (int i = 0; i < scanners.Length; i++)
			{
				scanners[i].Clear();
				scanners[i].Add(Vector3.Zero);
			}

			for (int c = 0; c < 100; c++)
			{
				for (int i = 1; i < scanners.Length; i++)
				{
					for (int j = 0; j < scanners.Length; j++)
					{
						if (scannerDiff.TryGetValue((i, j), out var diff))
						{
							foreach (var p in NumberedRotation(scanners[i], diff.rotation))
							{
								scanners[j].Add(p + diff.offset);
							}
						}
					}
				}
			}

			return scanners[0].SelectMany(f => scanners[0].Select(t => f.ManhattanDistance(t))).Max();
		}

		public IEnumerable<(int, HashSet<Vector3>)> AllRotations(HashSet<Vector3> set)
		{
			yield return (0, set);
			yield return (1, RotateZ(set, 1));
			yield return (2, RotateZ(set, 2));
			yield return (3, RotateZ(set, 3));

			set = RotateY(set, 1);
			yield return (4, set.ToHashSet());
			yield return (5, RotateX(set, 1));
			yield return (6, RotateX(set, 2));
			yield return (7, RotateX(set, 3));

			set = RotateY(set, 1);
			yield return (8, set.ToHashSet());
			yield return (9, RotateZ(set, 1));
			yield return (10, RotateZ(set, 2));
			yield return (11, RotateZ(set, 3));

			set = RotateY(set, 1);
			yield return (12, set.ToHashSet());
			yield return (13, RotateX(set, 1));
			yield return (14, RotateX(set, 2));
			yield return (15, RotateX(set, 3));

			set = RotateX(RotateY(set, 1), 1);
			yield return (16, set.ToHashSet());
			yield return (17, RotateY(set, 1));
			yield return (18, RotateY(set, 2));
			yield return (19, RotateY(set, 3));

			set = RotateZ(set, 2);
			yield return (20, set.ToHashSet());
			yield return (21, RotateY(set, 1));
			yield return (22, RotateY(set, 2));
			yield return (23, RotateY(set, 3));
		}
		public HashSet<Vector3> NumberedRotation(HashSet<Vector3> set, int index) => index switch
		{
			0 => set,
			1 => RotateZ(set, 1),
			2 => RotateZ(set, 2),
			3 => RotateZ(set, 3),

			4 => RotateY(set, 1),
			5 => RotateX(RotateY(set, 1), 1),
			6 => RotateX(RotateY(set, 1), 2),
			7 => RotateX(RotateY(set, 1), 3),

			8 => RotateY(set, 2),
			9 => RotateZ(RotateY(set, 2), 1),
			10 => RotateZ(RotateY(set, 2), 2),
			11 => RotateZ(RotateY(set, 2), 3),

			12 => RotateY(set, 3),
			13 => RotateX(RotateY(set, 3), 1),
			14 => RotateX(RotateY(set, 3), 2),
			15 => RotateX(RotateY(set, 3), 3),

			16 => RotateX(set, 1),
			17 => RotateY(RotateX(set, 1), 1),
			18 => RotateY(RotateX(set, 1), 2),
			19 => RotateY(RotateX(set, 1), 3),

			20 => RotateZ(RotateX(set, 1), 2),
			21 => RotateY(RotateZ(RotateX(set, 1), 2), 1),
			22 => RotateY(RotateZ(RotateX(set, 1), 2), 2),
			23 => RotateY(RotateZ(RotateX(set, 1), 2), 3),
			_ => throw new Exception("You dumb!")
		};

		public HashSet<Vector3> RotateX(HashSet<Vector3> set, int steps)
		{
			steps = MathM.Mod(steps, 4);
			if (steps == 0)
			{
				return set;
			}

			return set.Select(v => steps switch
				{
					1 => new Vector3(v.X, -v.Z, v.Y),
					2 => new Vector3(v.X, -v.Y, -v.Z),
					3 => new Vector3(v.X, v.Z, -v.Y),
					_ => throw new Exception("Impossible")
				})
				.ToHashSet();
		}
		public HashSet<Vector3> RotateY(HashSet<Vector3> set, int steps)
		{
			steps = MathM.Mod(steps, 4);
			if (steps == 0)
			{
				return set;
			}

			return set.Select(v => steps switch
				{
					1 => new Vector3(v.Z, v.Y, -v.X),
					2 => new Vector3(-v.X, v.Y, -v.Z),
					3 => new Vector3(-v.Z, v.Y, v.X),
					_ => throw new Exception("Impossible")
				})
				.ToHashSet();
		}
		public HashSet<Vector3> RotateZ(HashSet<Vector3> set, int steps)
		{
			steps = MathM.Mod(steps, 4);
			if (steps == 0)
			{
				return set;
			}

			return set.Select(v => steps switch
				{
					1 => new Vector3(-v.Y, v.X, v.Z),
					2 => new Vector3(-v.X, -v.Y, v.Z),
					3 => new Vector3(v.Y, -v.X, v.Z),
					_ => throw new Exception("Impossible")
				})
				.ToHashSet();
		}
	}
}
