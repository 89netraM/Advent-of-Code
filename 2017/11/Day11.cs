using System;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(11)]
	public class Day11
	{
		[Part(1)]
		public object Part1(string input)
		{
			string[] steps = input.Split(',');

			Vector3 target = Vector3.Zero;
			for (int i = 0; i < steps.Length; i++)
			{
				target = target + steps[i] switch
				{
					"n" => new Vector3(0, 1, -1),
					"s" => new Vector3(0, -1, 1),
					"ne" => new Vector3(1, 0, -1),
					"se" => new Vector3(1, -1, 0),
					"nw" => new Vector3(-1, 1, 0),
					"sw" => new Vector3(-1, 0, 1),
					_ => throw new Exception("Invalid direction")
				};
			}

			return Vector3.Zero.ManhattanDistance(target) / 2L;
		}

		[Part(2)]
		public object Part2(string input)
		{
			string[] steps = input.Split(',');
			long max = 0;

			Vector3 target = Vector3.Zero;
			for (int i = 0; i < steps.Length; i++)
			{
				target = target + steps[i] switch
				{
					"n" => new Vector3(0, 1, -1),
					"s" => new Vector3(0, -1, 1),
					"ne" => new Vector3(1, 0, -1),
					"se" => new Vector3(1, -1, 0),
					"nw" => new Vector3(-1, 1, 0),
					"sw" => new Vector3(-1, 0, 1),
					_ => throw new Exception("Invalid direction")
				};
				long dist = Vector3.Zero.ManhattanDistance(target) / 2L;
				max = Math.Max(max, dist);
			}

			return max;
		}
	}
}
