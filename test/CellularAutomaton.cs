using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class CellularAutomatonTest
	{
		#region 2020 Day 17
		enum State
		{
			Inactive,
			Active,
		}

		[TestMethod]
		public void CellularAutomaton_2020Day17Part1()
		{
			Dictionary<Vector3, State> input = new Dictionary<Vector3, State>
			{
				[new Vector3(3, 0, 0)] = State.Active,
				[new Vector3(6, 0, 0)] = State.Active,

				[new Vector3(2, 1, 0)] = State.Active,
				[new Vector3(3, 1, 0)] = State.Active,
				[new Vector3(5, 1, 0)] = State.Active,
				[new Vector3(6, 1, 0)] = State.Active,

				[new Vector3(2, 2, 0)] = State.Active,

				[new Vector3(4, 3, 0)] = State.Active,

				[new Vector3(0, 4, 0)] = State.Active,
				[new Vector3(2, 4, 0)] = State.Active,
				[new Vector3(3, 4, 0)] = State.Active,
				[new Vector3(7, 4, 0)] = State.Active,

				[new Vector3(0, 5, 0)] = State.Active,
				[new Vector3(1, 5, 0)] = State.Active,
				[new Vector3(2, 5, 0)] = State.Active,
				[new Vector3(3, 5, 0)] = State.Active,
				[new Vector3(6, 5, 0)] = State.Active,
				[new Vector3(7, 5, 0)] = State.Active,

				[new Vector3(3, 6, 0)] = State.Active,
				[new Vector3(4, 6, 0)] = State.Active,
				[new Vector3(6, 6, 0)] = State.Active,

				[new Vector3(0, 7, 0)] = State.Active,
				[new Vector3(2, 7, 0)] = State.Active,
				[new Vector3(4, 7, 0)] = State.Active,
			};

			CellularAutomaton<Vector3, State> ca = new CellularAutomaton<Vector3, State>(input)
			{
				Neighborhood = NeighborhoodKind.Moore,
				[State.Active] = static c => c[State.Active] == 2 || c[State.Active] == 3 ? State.Active : State.Inactive,
				[State.Inactive] = static c => c[State.Active] == 3 ? State.Active : State.Inactive,
			};

			for (int i = 0; i < 6; i++)
			{
				ca.Step();
			}

			Assert.AreEqual(247, ca.Count(static kvp => kvp.Value == State.Active));
		}

		[TestMethod]
		public void CellularAutomaton_2020Day17Part2()
		{
			Dictionary<Vector4, State> input = new Dictionary<Vector4, State>
			{
				[new Vector4(3, 0, 0, 0)] = State.Active,
				[new Vector4(6, 0, 0, 0)] = State.Active,

				[new Vector4(2, 1, 0, 0)] = State.Active,
				[new Vector4(3, 1, 0, 0)] = State.Active,
				[new Vector4(5, 1, 0, 0)] = State.Active,
				[new Vector4(6, 1, 0, 0)] = State.Active,

				[new Vector4(2, 2, 0, 0)] = State.Active,

				[new Vector4(4, 3, 0, 0)] = State.Active,

				[new Vector4(0, 4, 0, 0)] = State.Active,
				[new Vector4(2, 4, 0, 0)] = State.Active,
				[new Vector4(3, 4, 0, 0)] = State.Active,
				[new Vector4(7, 4, 0, 0)] = State.Active,

				[new Vector4(0, 5, 0, 0)] = State.Active,
				[new Vector4(1, 5, 0, 0)] = State.Active,
				[new Vector4(2, 5, 0, 0)] = State.Active,
				[new Vector4(3, 5, 0, 0)] = State.Active,
				[new Vector4(6, 5, 0, 0)] = State.Active,
				[new Vector4(7, 5, 0, 0)] = State.Active,

				[new Vector4(3, 6, 0, 0)] = State.Active,
				[new Vector4(4, 6, 0, 0)] = State.Active,
				[new Vector4(6, 6, 0, 0)] = State.Active,

				[new Vector4(0, 7, 0, 0)] = State.Active,
				[new Vector4(2, 7, 0, 0)] = State.Active,
				[new Vector4(4, 7, 0, 0)] = State.Active,
			};

			CellularAutomaton<Vector4, State> ca = new CellularAutomaton<Vector4, State>(input)
			{
				Neighborhood = NeighborhoodKind.Moore,
				[State.Active] = static c => c[State.Active] == 2 || c[State.Active] == 3 ? State.Active : State.Inactive,
				[State.Inactive] = static c => c[State.Active] == 3 ? State.Active : State.Inactive,
			};

			for (int i = 0; i < 6; i++)
			{
				ca.Step();
			}

			Assert.AreEqual(1392, ca.Count(static kvp => kvp.Value == State.Active));
		}
		#endregion 2020 Day 17

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
		public void CellularAutomaton_2018Day18Part1()
		{
			Dictionary<Vector2, Area> input = Input2018Day18.Split("\r\n")
				.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Vector2(x, y))))
				.ToDictionary(static p => p.coord, static p => p.c switch { '.' => Area.Open, '|' => Area.Tree, '#' => Area.Lumberyard });

			CellularAutomaton<Vector2, Area> ca = new CellularAutomaton<Vector2, Area>(input)
			{
				Neighborhood = NeighborhoodKind.Moore,
				[Area.Open] = static c => c[Area.Tree] >= 3 ? Area.Tree : Area.Open,
				[Area.Tree] = static c => c[Area.Lumberyard] >= 3 ? Area.Lumberyard : Area.Tree,
				[Area.Lumberyard] = static c => c[Area.Lumberyard] >= 1 && c[Area.Tree] >= 1 ? Area.Lumberyard : Area.Open,
			};
			ca.ConfinementBounds = ca.Bounds();

			for (int i = 0; i < 10; i++)
			{
				ca.Step();
			}

			Assert.AreEqual(543312, ca.Count(static kvp => kvp.Value == Area.Tree) * ca.Count(static kvp => kvp.Value == Area.Lumberyard));
		}
		#endregion 2018 Day 18
	}
}
