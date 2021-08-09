using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class HugeTest
	{
		private class CloneableInt : IEquatable<CloneableInt>, ICloneable
		{
			public int Value { get; set; }
			public CloneableInt(int value) =>
				(Value) = (value);

			public object Clone() =>
				new CloneableInt(Value);

			public bool Equals(CloneableInt? other) =>
				other is CloneableInt o && Value == o.Value;
			public override bool Equals(object? other) =>
				other is CloneableInt o && Equals(o);
			public override int GetHashCode() =>
				HashCode.Combine(Value);

			public static implicit operator int(CloneableInt i) =>
				i.Value;
		}

		[TestMethod]
		public void Huge_BasicPeriod()
		{
			const long TotalSteps = 1_000_000_000L;
			long[] values = new long[] { 12L, 50L, 1L, 4L, 10L, 99L, 5L, 7L, 0L, 33L };
			long stepsTaken = 0L;

			long result = Huge.TakeSteps(
				TotalSteps,
				new CloneableInt(0),
				(state, i) =>
				{
					stepsTaken++;
					state.Value = MathM.Mod(state + 1, values.Length);
					return state;
				},
				(state, i) => values[MathM.Mod(state - 1, values.Length)]
			);

			Assert.AreEqual(values.Length + 1L, stepsTaken);
			Assert.AreEqual(values[0], result);
		}

		[TestMethod]
		public void Huge_BasicArithmeticProgression()
		{
			long[] values = new long[] { 0L, 2L, 5L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L };
			long stepsTaken = 0L;

			long result = Huge.TakeSteps(
				values.Length,
				new CloneableInt(0),
				(state, i) =>
				{
					stepsTaken++;
					state.Value++;
					return state;
				},
				(state, i) => values[state - 1],
				HugeSearchPattern.Progressions
			);

			Assert.IsTrue(stepsTaken < values.Length);
			Assert.AreEqual(values[^1], result);
		}

		[TestMethod]
		public void Huge_BasicGeometricProgression()
		{
			long[] values = new long[] { 0L, 2L, 5L, 7L, 14L, 28L, 56L, 112L, 224L, 448L, 896L, 1_792L };
			long stepsTaken = 0L;

			long result = Huge.TakeSteps(
				values.Length,
				new CloneableInt(0),
				(state, i) =>
				{
					stepsTaken++;
					state.Value++;
					return state;
				},
				(state, i) => values[state - 1]
			);

			Assert.IsTrue(stepsTaken < values.Length);
			Assert.AreEqual(values[^1], result);
		}

		#region 2018 Day 18
		private const string Input2018Day18 = @"#||.|...#.......#|#|....#.|#.#.|...#.|#........##.
...#...##.#...##|.....|..|....|......#||..#.|.##.#
..||..####|||.||#|....##|#.##|.#...##.|.|.||.#.##|
.||#..||.||..|||...|.....##..#..........||.#..#...
|...|...|.....#......||#|..|.....##.||.#..##|.#||#
|...#.|.||#|#..|..|..#|#|.##.....|...#|.#....#..#.
|...#...#|#.|#.#||....#..||.|..|.||.|.|.....#..|#|
..|..##..#..|###.||...|||#.#...#.#....##...|..|..|
..##...#.......#|#|#...#..##.#........|.|#......|.
|..|#..|##|...#....#.##|...#.....|...........|#..|
|....|.|#..##|.|##||.#.....|.#..#|.#.#|#|.......#|
||..#.##..##...#|#.#...#|.##..###.....#..#.|..|##|
#|#|..|.#.|.#.|.|...||#.|..#....#..|...||..|..#|.#
.###..|....|.#||#..##.#|..|||..##|.||....|.#.|....
|....#..#####......|||||##..|...#........#..|...##
..##........|...||......##.#.#...|..|#...#|....|..
||...#.#|..#||.....|#.#.|.....|.|..#.#.|....#.#..|
......#..##.|.#|..|||#.........|#.|#|.|...|.|#..#.
||...#..##|..|#.#|.....#.|...|.|...|.|.|..#.#..#.|
....#...#..###||.||.##....#....||..|...||.|..|...#
.#..#.#|....|.#.|#|.....#|.......||..##...#..#|.|.
......#||##..#...#..|..#.|....||...#...|.#...#.#.|
....||.....|.|#..||.#||....#..#.|.#...||.|.|....#.
#.|.#.#..|.....|||.......#.#.#.|.#......#....||#.|
|....##......|.||#.|...#.#...#|...|.||..|.#..|....
||##..#......#|.#|.||||#.#...#.#.###.|#..#####....
#.|..|#|#....#.|..||#|###|..|.|...|.#..|.|.#..##||
..#..#..#...|..#.|....|||..#||.|.....|..#......||.
||....||..|#||....|..#.##..|||#|.#|.#.||.||..||...
.|.|.|##..|.##..|....#.#.#||....#...#|.#....##.#.|
|.#....|.#...#..#...|.#.|#...|||..|#.#...|...#...|
..|##...#..#.....|.|.#....#....||.#.|.#......|||##
...||#...#|#........#..#.|.#...|||.......###..#...
#.|.|#.#..#...#.....#..#....|.|..#...#.###.....#|#
#...|.#..|.|||..||...##..|.....|##...||....###.##.
##|.#..##..|.|.#.#.|#..##...|..|.#.||#..|...#|..##
.#...#....##....||.#.|.|.|..|.|.###......#.###.|.#
|.|.#..|#.#|..#.#...|.#|||#.#.....#.|#|||..#..|#..
...|#...||#.#.|....|......|..#|.|.......#.##..##..
..#....|.#.|....#|##...#||##......|#.|..|.|..#.|..
||.|.#|.|#|#.......|.|#...|.#...|#..|....###..#.##
..|.#|...|.#.#.#.#.#|..#...#..#|...#...#.......#..
##|.....||#...#...|...|#...#.#......||..........|#
..##.##....#.|.##..|#....|#...#|....#.##|.|#.||...
.|....#...|...#..|##.###|#.#..|...||||.#.#.|.#....
|..#|....|...#.....|..#|..||.|......#..||...#|.|.|
|...#.#||..#..||.....|.....#|||...|...#|..|#.#|#.#
.|.........#..|###......||.|#|..||#.|..|.|.....|.#
||#..|..|.||#|.#|##|...#.##..#||.|....##....|.#||.
.#....|#.#....|.|.|#..#......|||...#....|.........";

		enum Area
		{
			Open,
			Tree,
			Lumberyard,
		}

		[TestMethod]
		public void Huge_2018Day18Part2()
		{
			const long Steps = 1_000_000_000L;
			long stepsTaken = 0L;

			Dictionary<Vector2, Area> input = Input2018Day18.Split("\r\n")
				.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Vector2(x, y))))
				.ToDictionary(
					static p => p.coord,
					static p => p.c switch
					{
						'.' => Area.Open,
						'|' => Area.Tree,
						'#' => Area.Lumberyard,
						_ => throw new Exception("Invalid character"),
					}
				);

			CellularAutomaton<Vector2, Area> ca = new CellularAutomaton<Vector2, Area>(input)
			{
				Neighborhood = NeighborhoodKind.Moore,
				[Area.Open] = static c => c[Area.Tree] >= 3 ? Area.Tree : Area.Open,
				[Area.Tree] = static c => c[Area.Lumberyard] >= 3 ? Area.Lumberyard : Area.Tree,
				[Area.Lumberyard] = static c => c[Area.Lumberyard] >= 1 && c[Area.Tree] >= 1 ? Area.Lumberyard : Area.Open,
			};
			ca.ConfinementBounds = ca.Bounds();

			long result = Huge.TakeSteps(
				Steps,
				ca,
				(ca, i) =>
				{
					stepsTaken++;
					ca.Step();
					return ca;
				},
				(ca, i) => ca.Count(static kvp => kvp.Value == Area.Tree) * ca.Count(static kvp => kvp.Value == Area.Lumberyard)
			);

			Assert.IsTrue(stepsTaken < Steps);
			Assert.AreEqual(199064, result);
		}
		#endregion 2018 Day 18

		#region 2018 Day 12
		private const string Input2018Day12 = @"initial state: ##.##.##..#..#.#.#.#...#...#####.###...#####.##..#####.#..#.##..#..#.#...#...##.##...#.##......####.

##.#. => #
#.#.. => #
##... => .
...## => #
###.# => #
#.##. => #
#.### => #
####. => #
.#..# => #
...#. => .
#..#. => .
#.#.# => .
.##.# => .
..#.. => .
.#.## => #
..##. => .
.#.#. => #
#..## => #
..#.# => #
#.... => .
..### => .
#...# => .
##### => #
###.. => #
....# => .
##.## => #
.#### => .
..... => .
##..# => #
.##.. => .
.###. => .
.#... => #";

		private class CloneableHashSet<T> : HashSet<T>, IEquatable<CloneableHashSet<T>>, ICloneable
		{
			public CloneableHashSet() : base() { }
			public CloneableHashSet(IEnumerable<T> collection) : base(collection) { }

			public object Clone() =>
				new CloneableHashSet<T>(this);

			public bool Equals(CloneableHashSet<T>? other) =>
				other is CloneableHashSet<T> o && Count == o.Count && this.SetEquals(o);
			public override bool Equals(object? other) =>
				other is CloneableHashSet<T> o && Equals(o);
			public override int GetHashCode()
			{
				HashCode h = new HashCode();
				foreach (T t in this)
				{
					h.Add(t);
				}
				return h.ToHashCode();
			}
		}

		private record Rule(bool m2, bool m1, bool c, bool p1, bool p2, bool n);
		static bool MatchRule(Rule r, bool m2, bool m1, bool c, bool p1, bool p2) =>
			m2 == r.m2 && m1 == r.m1 && c == r.c && p1 == r.p1 && p2 == r.p2;

		[TestMethod]
		public void Huge_2018Day12()
		{
			const long Steps = 50_000_000_000L;
			long stepsTaken = 0L;

			string[] input = Input2018Day12.Split("\r\n");

			Rule[] rules = input.Skip(2)
				.Select(static l => new Rule(l[0] == '#', l[1] == '#', l[2] == '#', l[3] == '#', l[4] == '#', l[9] == '#'))
				.ToArray();

			long result = Huge.TakeSteps(
				Steps,
				new CloneableHashSet<long>(input[0].Select(static (c, i) => (c, i)).Where(static p => p.c == '#').Select(static p => (long)p.i - 15L)),
				(current, _) =>
				{
					stepsTaken++;

					CloneableHashSet<long> next = new CloneableHashSet<long>();
					long min = current.Min() - 2;
					long max = current.Max() + 2;
					for (long x = min; x <= max; x++)
					{
						if (rules.FirstOrDefault(r => MatchRule(r, current.Contains(x - 2), current.Contains(x - 1), current.Contains(x), current.Contains(x + 1), current.Contains(x + 2))) is Rule r && r.n)
						{
							next.Add(x);
						}
					}
					return next;
				},
				static (current, _) => current.Sum()
			);

			Assert.IsTrue(stepsTaken < Steps);
			Assert.AreEqual(3099999999491, result);
		}
		#endregion 2018 Day 12
	}
}
