using AoC.Library;

namespace AoC.Year2021
{
	[Day(2)]
	public class Day2
	{
		[Part(1)]
		public object Part1(string input)
		{
			long hor = 0L;
			long vert = 0L;
			foreach (var line in input.Lines())
			{
				if (line.StartsWith("forward"))
				{
					hor += long.Parse(line.Substring(8));
				}
				else if (line.StartsWith("up"))
				{
					vert -= long.Parse(line.Substring(3));
				}
				else if (line.StartsWith("down"))
				{
					vert += long.Parse(line.Substring(5));
				}
			}
			return hor * vert;
		}

		[Part(2)]
		public object Part2(string input)
		{
			long hor = 0L;
			long vert = 0L;
			long aim = 0L;
			foreach (var line in input.Lines())
			{
				if (line.StartsWith("forward"))
				{
					hor += long.Parse(line.Substring(8));
					vert += aim * long.Parse(line.Substring(8));
				}
				else if (line.StartsWith("up"))
				{
					aim -= long.Parse(line.Substring(3));
				}
				else if (line.StartsWith("down"))
				{
					aim += long.Parse(line.Substring(5));
				}
			}
			return hor * vert;
		}
	}
}
