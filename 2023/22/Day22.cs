using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2023;

[Day(22)]
public class Day22
{
	[Part(1)]
	public object Part1(string input)
	{
		var cubes = input.Lines()
			.Extract<(Vector3 from, Vector3 to)>(@"((-?\d+),(-?\d+),(-?\d+))~((-?\d+),(-?\d+),(-?\d+))")
			.Select((c, i) => new Cube(c.from.MinParts(c.to), c.from.MaxParts(c.to), i))
			.ToList();
		cubes.Sort();
		var (up, down) = MakeUpDownTrees(cubes);
		return up.Values.Count(a => a.Count == 0 || a.All(i => down[i].Count > 1));
	}

	[Part(2)]
	public object Part2(string input)
	{
		var cubes = input.Lines()
			.Extract<(Vector3 from, Vector3 to)>(@"((-?\d+),(-?\d+),(-?\d+))~((-?\d+),(-?\d+),(-?\d+))")
			.Select((c, i) => new Cube(c.from.MinParts(c.to), c.from.MaxParts(c.to), i))
			.ToList();
		cubes.Sort();
		var (up, down) = MakeUpDownTrees(cubes);
		return cubes.Sum(c => CountChainReaction(up, down, c.Id));
	}

	private static (Dictionary<int, List<int>>, Dictionary<int, List<int>>) MakeUpDownTrees(IReadOnlyCollection<Cube> cubes)
	{
		var up = new Dictionary<int, List<int>>(cubes.Count);
		var down = new Dictionary<int, List<int>>(cubes.Count);
		var nextCubes = new List<Cube>(cubes.Count);
		var beneeth = new List<Cube>();
		foreach (var cube in cubes)
		{
			foreach (var other in nextCubes)
			{
				if (!cube.OverlappingXY(other)) { continue; }
				if (beneeth.Count == 0 || beneeth.All(t => t.To.Z == other.To.Z))
				{
					beneeth.Add(other);
				}
				else if (beneeth.All(t => t.To.Z < other.To.Z))
				{
					beneeth.Clear();
					beneeth.Add(other);
				}
			}
			up.Add(cube.Id, new List<int>());
			down.Add(cube.Id, beneeth.Select(c => c.Id).ToList());
			foreach (var b in beneeth)
			{
				up[b.Id].Add(cube.Id);
			}
			nextCubes.Add(cube.MoveToFloor(beneeth.Count == 0 ? 0 : beneeth[0].To.Z));
			beneeth.Clear();
		}
		return (up, down);
	}

	private static long CountChainReaction(Dictionary<int, List<int>> up, Dictionary<int, List<int>> down, int start)
	{
		var deleted = new HashSet<int> { start };
		var toDelete = new Queue<int>();
		up[start].ForEach(toDelete.Enqueue);

		while (toDelete.TryDequeue(out var cube))
		{
			if (down[cube].All(deleted.Contains))
			{
				deleted.Add(cube);
				up[cube].ForEach(toDelete.Enqueue);
			}
		}

		return deleted.Count - 1;
	}

	private record Cube(Vector3 From, Vector3 To, int Id) : IComparable<Cube>
	{
		public bool OverlappingXY(Cube other)
		{
				//    +-----+
				//    |     |
				// +--|--T  |
				// |  |  |  |
				// |  P--+--+
				// |     |
				// F-----+
			return IsWithin(From.XY, To.XY, other.From.XY)
				//    +-----T
				//    |     |
				// +--|--P  |
				// |  |  |  |
				// |  F--+--+
				// |     |
				// +-----+
				|| IsWithin(From.XY, To.XY, other.To.XY)
				// +-----T
				// |     |
				// |  P--+--+
				// |  |  |  |
				// F--+--+  |
				//    |     |
				//    +-----+
				|| IsWithin(From.XY, To.XY, new(other.From.X, other.To.Y))
				// +-----+
				// |     |
				// |  +--+--T
				// |  |  |  |
				// +--+--P  |
				//    |     |
				//    F-----+
				|| IsWithin(From.XY, To.XY, new(other.To.X, other.From.Y))
				//   +-+
				// +-+-+-T
				// | | | |
				// | | | |
				// | | | |
				// F-+-+-+
				//   +-+
				|| From.X <= other.From.X && other.To.X <= To.X
					&& other.From.Y <= From.Y && To.Y <= other.To.Y
				//   +-+-+-T
				// +-+-----+-+
				// | |     | |
				// +-+-----+-+
				//   F-+-+-+
				|| From.Y <= other.From.Y && other.To.Y <= To.Y
					&& other.From.X <= From.X && To.X <= other.To.X;

			static bool IsWithin(Vector2 min, Vector2 max, Vector2 point) =>
				min.X <= point.X && point.X <= max.X
					&& min.Y <= point.Y && point.Y <= max.Y;
		}

		public Cube MoveToFloor(long floor) =>
			new(new(From.X, From.Y, floor + 1), new(To.X, To.Y, floor + 1 + (To.Z - From.Z)), Id);

		public int CompareTo(Cube other) =>
			From.Z.CompareTo(other.From.Z);
	}
}
