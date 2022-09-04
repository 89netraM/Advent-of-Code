using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2015
{
	[Day(14)]
	public class Day14
	{
		[Part(1)]
		public long Part1(string input)
		{
			var reindeers = input
				.Lines()
				.Extract<Reindeer>(@"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")
				.ToArray();

			for (int time = 0; time < 2503; time++)
			{
				foreach (var reindeer in reindeers)
				{
					reindeer.Tick(time);
				}
			}

			return reindeers.Max(r => r.Distance);
		}

		[Part(2)]
		public long Part2(string input)
		{
			var reindeers = input
				.Lines()
				.Extract<Reindeer>(@"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")
				.ToArray();

			for (int time = 0; time < 2503; time++)
			{
				foreach (var reindeer in reindeers)
				{
					reindeer.Tick(time);
				}

				foreach (var leader in reindeers.GroupBy(r => r.Distance).MaxBy(g => g.Key))
				{
					leader.Score++;
				}
			}

			return reindeers.Max(r => r.Score);
		}

		private record Reindeer(string Name, long Speed, long Flying, long Sleeping)
		{
			public long Distance { get; private set; } = 0;
			public long Score { get; set; } = 0;
			private long period => Flying + Sleeping;

			private bool IsFlying(long time) => MathM.Mod(time, period) < Flying;
			public void Tick(long time)
			{
				if (IsFlying(time))
				{
					Distance += Speed;
				}
			}
		}
	}
}
