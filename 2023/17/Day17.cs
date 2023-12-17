using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(17)]
public class Day17
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = input.ToMapLong();
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));

		BFS.Search(
			(pos: Vector2.Zero, dir: Vector2.Zero, steps: 0),
			n => n.pos.NeighborsVonNeumann()
				.Where(ne => map.ContainsKey(ne))
				.Select(ne => (pos: ne, dir: ne - n.pos))
				.Where(ne => ne.dir != -n.dir)
				.Select(ne => (ne.pos, ne.dir, steps: n.dir == ne.dir ? n.steps + 1 : 1))
				.Where(ne => ne.steps < 4)
				.Select(ne => (ne, map[ne.pos])),
			n => n.pos == max,
			out var path);

		return path.Last().Item2;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = input.ToMapLong();
		var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));

		BFS.Search(
			(pos: Vector2.Zero, dir: Vector2.Zero, steps: 10),
			n => n.steps < 4
				? new (Vector2 pos, Vector2 dir, int steps)[]{(n.pos + n.dir, n.dir, n.steps + 1)}
					.Where(ne => map.ContainsKey(ne.pos))
					.Select(ne => (ne, map[ne.pos]))
				: n.pos.NeighborsVonNeumann()
					.Where(ne => map.ContainsKey(ne))
					.Select(ne => (pos: ne, dir: ne - n.pos))
					.Where(ne => ne.dir != -n.dir)
					.Select(ne => (ne.pos, ne.dir, steps: n.dir == ne.dir ? n.steps + 1 : 1))
					.Where(ne => ne.steps <= 10)
					.Select(ne => (ne, map[ne.pos])),
			n => n.pos == max && n.steps >= 4,
			out var path);

		return path.Last().Item2;
	}
}
