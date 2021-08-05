using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using static AoC.Library.Functional;

namespace AoC.Library.Test
{
	[TestClass]
	public class BFSTest
	{
		#region 2018 Day 22 Part 2
		private static Func<Vector2, Vector2, long, long> GeologyIndex = Curry(Memoize<(Vector2, Vector2, long), long>(static p =>
		{
			var (c, target, depth) = p;
			if (c.X == 0L && c.Y == 0L) return 0L;
			if (c.X == target.X && c.Y == target.Y) return 0L;
			if (c.Y == 0L) return c.X * 16807L;
			if (c.X == 0L) return c.Y * 48271L;
			return ErosionLevel(new Vector2(c.X - 1L, c.Y), target, depth) * ErosionLevel(new Vector2(c.X, c.Y - 1L), target, depth);
		}));

		private enum GroundType : long
		{
			Rocky = 0,
			Wet = 1,
			Narrow = 2,
		}
		private static bool IsEquipmentAllowed(GroundType t, Equipment e) => t switch
		{
			GroundType.Rocky => e == Equipment.ClimbingGear || e == Equipment.Torch,
			GroundType.Wet => e == Equipment.ClimbingGear || e == Equipment.Neither,
			GroundType.Narrow => e == Equipment.Torch || e == Equipment.Neither,
			_ => throw new ArgumentException($"Unknown ground type {t}"),
		};
		private static Equipment OtherEquipment(GroundType t, Equipment e) => (t, e) switch
		{
			(GroundType.Rocky, Equipment.ClimbingGear) => Equipment.Torch,
			(GroundType.Rocky, Equipment.Torch) => Equipment.ClimbingGear,
			(GroundType.Wet, Equipment.ClimbingGear) => Equipment.Neither,
			(GroundType.Wet, Equipment.Neither) => Equipment.ClimbingGear,
			(GroundType.Narrow, Equipment.Torch) => Equipment.Neither,
			(GroundType.Narrow, Equipment.Neither) => Equipment.Torch,
			_ => throw new ArgumentException($"Unknown ground/equipment combination {(t, e)}")
		};

		private static long ErosionLevel(Vector2 c, Vector2 target, long depth) =>
			MathM.Mod(GeologyIndex(c, target, depth) + depth, 20183L);

		private static GroundType GeologyType(Vector2 c, Vector2 target, long depth) =>
			(GroundType)MathM.Mod(ErosionLevel(c, target, depth), 3L);

		private enum Equipment
		{
			Neither,
			Torch,
			ClimbingGear,
		}
		#endregion 2018 Day 22 Part 2

		[TestMethod]
		public void BFS_2018Day22Part2()
		{
			long depth = 3879;
			Vector2 target = new Vector2(8, 713);

			HashSet<(Vector2, Equipment)> visited = new HashSet<(Vector2, Equipment)>();

			bool success = BFS.Search(
				(coord: new Vector2(0, 0), equipment: Equipment.Torch),
				current => current.coord.NeighborsVonNeumann()
					.Select(coord => ((coord, current.equipment), 1L))
					.Where(n => n.Item1.coord.X >= 0 && n.Item1.coord.Y >= 0 &&
						IsEquipmentAllowed(GeologyType(n.Item1.coord, target, depth), n.Item1.equipment))
					.Append(((current.coord, OtherEquipment(GeologyType(current.coord, target, depth), current.equipment)), 7L)),
				current => current.coord == target && current.equipment == Equipment.Torch,
				out IEnumerable<((Vector2 coord, Equipment equipment) step, long time)> path,
				(n, _) => Assert.IsTrue(visited.Add(n), $"Visited {n} twice!")
			);

			Assert.IsTrue(success);
			Assert.AreEqual(982, path.Last().time);
		}

		private const string AsciiMap =
			"#######\n" +
			"#o#   #\n" +
			"# # # #\n" +
			"#   # #\n" +
			"# ###x#\n" +
			"#~~~~~#\n" +
			"#######\n";
		private static readonly IEnumerable<(Vector2 coord, char c)> AsciiMapCoords =
			AsciiMap.Split('\n').SelectMany(static (l, y) => l.Select((c, x) => (new Vector2(x, y), c)));
		private static readonly Vector2 AsciiMapStart = AsciiMapCoords.Single(static p => p.c == 'o').coord;
		private static readonly Vector2 AsciiMapGoal = AsciiMapCoords.Single(static p => p.c == 'x').coord;
		private static readonly HashSet<Vector2> AsciiMapWalls = AsciiMapCoords
			.Where(static p => p.c == '#')
			.Select(static p => p.coord)
			.ToHashSet();
		private static readonly HashSet<Vector2> AsciiMapWater = AsciiMapCoords
			.Where(static p => p.c == '~')
			.Select(static p => p.coord)
			.ToHashSet();

		[TestMethod]
		public void BFS_Search()
		{
			HashSet<Vector2> visited = new HashSet<Vector2>();

			bool success = BFS.Search(
				AsciiMapStart,
				current => current.NeighborsVonNeumann()
					.Where(Not<Vector2>(AsciiMapWalls.Contains)),
				static current => current == AsciiMapGoal,
				out IEnumerable<Vector2> path,
				n => Assert.IsTrue(visited.Add(n), $"Visited {n} twice!")
			);

			Assert.IsTrue(success);
			Assert.IsTrue(path.SequenceEqual(new[] {
				new Vector2(1, 2),
				new Vector2(1, 3),
				new Vector2(1, 4),
				new Vector2(1, 5),
				new Vector2(2, 5),
				new Vector2(3, 5),
				new Vector2(4, 5),
				new Vector2(5, 5),
				AsciiMapGoal
			}), "Wrong path: " + String.Join(", ", path));
		}

		[TestMethod]
		public void BFS_SearchWithCost()
		{
			HashSet<Vector2> visited = new HashSet<Vector2>();

			bool success = BFS.Search(
				AsciiMapStart,
				static current => current.NeighborsVonNeumann()
					.Where(Not<Vector2>(AsciiMapWalls.Contains))
					.Select(static n => (n, AsciiMapWater.Contains(n) ? 2L : 1L)),
				static current => current == AsciiMapGoal,
				out IEnumerable<(Vector2, long)> path,
				(n, _) => Assert.IsTrue(visited.Add(n), $"Visited {n} twice!")
			);

			Assert.IsTrue(success);
			Assert.IsTrue(path.SequenceEqual(new[] {
				(new Vector2(1, 2), 1L),
				(new Vector2(1, 3), 2L),
				(new Vector2(2, 3), 3L),
				(new Vector2(3, 3), 4L),
				(new Vector2(3, 2), 5L),
				(new Vector2(3, 1), 6L),
				(new Vector2(4, 1), 7L),
				(new Vector2(5, 1), 8L),
				(new Vector2(5, 2), 9L),
				(new Vector2(5, 3), 10L),
				(AsciiMapGoal, 11L)
			}), "Wrong path: " + String.Join(", ", path));
		}
	}
}
