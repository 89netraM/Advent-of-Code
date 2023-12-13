using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2023;

[Day(13)]
public class Day13
{
	[Part(1)]
	public object Part1(string input) =>
		input.Paragraphs()
			.Select(s => s.Lines().Select(l => l.ToCharArray()))
			.Select(FindMirror)
			.Sum();

	[Part(2)]
	public object Part2(string input) =>
		input.Paragraphs()
			.Select(s => s.Lines().Select(l => l.ToCharArray()))
			.AsParallel()
			.Select(FindMirrorWithSmudge)
			.Sum();

	private static long FindMirror(IEnumerable<char[]> points) =>
		FindMirrorColumn(points) + 100 * FindMirrorRow(points);

	private static long FindMirrorWithSmudge(IEnumerable<char[]> ps)
	{
		var points = ps.ToArray();
		var col = (int)FindMirrorColumn(points);
		var row = (int)FindMirrorRow(points);
		for (int y = 0; y < points.Length; y++)
		{
			for (int x = 0; x < points[y].Length; x++)
			{
				var og = points[y][x];
				points[y][x] = og == '.' ? '#' : '.';
				var m = FindMirrorExcept(points, col, row);
				if (m > 0) { return m; }
				points[y][x] = og;
			}
		}
		throw new Exception("Noooooo!");
	}

	private static long FindMirrorExcept(IEnumerable<char[]> points, int col, int row) =>
		FindMirrorColumn(points, col) + 100 * FindMirrorRow(points, row);

	private static long FindMirrorColumn(IEnumerable<char[]> points, int col = 0)
	{
		return points.Select(ColumnsForRow)
			.Aggregate((a, b) => a.Also(a => a.IntersectWith(b)))
			.SingleOrDefault();

		HashSet<long> ColumnsForRow(char[] points)
		{
			var columns = new HashSet<long>();
			for (int x = 1; x < points.Length; x++)
			{
				if (x == col) { continue; }
				var length = int.Min(x, points.Length - x);
				var left = points[..x].Reverse().Take(length);
				var right = points[x..][..length];
				if (left.SequenceEqual(right))
				{
					columns.Add(x);
				}
			}
			return columns;
		}
	}

	private static long FindMirrorRow(IEnumerable<char[]> points, int row = 0) =>
		points
			.Transpose()
			.Select(s => s.ToArray())
			.Let(p => FindMirrorColumn(p, row));
}
