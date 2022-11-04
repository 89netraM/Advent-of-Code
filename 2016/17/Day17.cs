using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2016;

[Day(17)]
public class Day17
{
	[Part(1)]
	public object Part1(string input)
	{
		BFS.Search(
			(h: input, p: Vector2.Zero),
			GetNexts,
			p => p.p == goal,
			out var path);

		return path.Last().h.Substring(input.Length);
	}

	[Part(2)]
	public object Part2(string input)
	{
		int length = 0;
		BFS.Search(
			(h: input, p: Vector2.Zero),
			GetNexts,
			p => false,
			out _,
			p => {
				if (p.p == goal)
				{
					length = Math.Max(length, p.h.Length - input.Length);
				}
			});

		return length;
	}

	private static readonly Vector2 goal = new Vector2(3, 3);
	private static readonly string allowable = "bcdef";
	public IEnumerable<(string, Vector2)> GetNexts((string, Vector2) node)
	{
		var (code, pos) = node;
		if (pos == goal)
		{
			yield break;
		}

		var hash = Hash.MD5(code);

		if (pos.Y > 0 && allowable.Contains(hash[0]))
		{
			yield return (code + "U", pos + Vector2.Up);
		}
		if (pos.Y < 3 && allowable.Contains(hash[1]))
		{
			yield return (code + "D", pos + Vector2.Down);
		}
		if (pos.X > 0 && allowable.Contains(hash[2]))
		{
			yield return (code + "L", pos + Vector2.Left);
		}
		if (pos.X < 3 && allowable.Contains(hash[3]))
		{
			yield return (code + "R", pos + Vector2.Right);
		}
	}
}
