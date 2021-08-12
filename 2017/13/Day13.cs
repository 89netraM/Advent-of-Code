using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(13)]
	public class Day13
	{
		[Part(1)]
		public object Part1(string input)
		{
			var firewall = input.Lines()
				.Select(Parsing.PatternParser<int, int>(@"(\d+): (\d+)"))
				.ToDictionary(static p => (int)p?.Item1, p => (height: (int)p?.Item2, pos: 0, dir: 1));

			int end = firewall.Keys.Max();

			int severity = 0;

			for (int i = 0; i <= end; i++)
			{
				if (firewall.TryGetValue(i, out var f) && f.pos == 0)
				{
					severity += i * f.height;
				}
				Move(firewall);
			}

			return severity;
		}

		private static void Move(Dictionary<int, (int, int, int)> firewall)
		{
			foreach (var key in firewall.Keys.ToList())
			{
				var (height, pos, dir) = firewall[key];
				if (pos == 0)
				{
					dir = 1;
				}
				else if (pos == height - 1)
				{
					dir = -1;
				}
				firewall[key] = (height, pos + dir, dir);
			}
		}

		[Part(2)]
		public object Part2(string input)
		{
			var ogFirewall = input.Lines()
				.Select(Parsing.PatternParser<int, int>(@"(\d+): (\d+)"))
				.ToDictionary(static p => (int)p?.Item1, p => (int)p?.Item2);

			int end = ogFirewall.Keys.Max();

			int delay = 1;
			while (true)
			{
				if (Test(ogFirewall, delay, end))
				{
					return delay;
				}
				delay++;
			}
		}

		private static bool Test(IReadOnlyDictionary<int, int> firewall, int delay, int end)
		{
			foreach (var kvp in firewall)
			{
				if (MathM.Mod(delay + kvp.Key, kvp.Value * 2 - 2) == 0)
				{
					return false;
				}
			}
			return true;
		}
	}
}
