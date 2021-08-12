using AoC.Library;

namespace AoC.Year2017
{
	[Day(15)]
	public class Day15
	{
		private class Generator
		{
			private readonly long factor;
			private readonly long mult;
			public long Previous { get; private set; } = 0;

			public Generator(long starting, long factor, long mult) =>
				(Previous, this.factor, this.mult) = (starting, factor, mult);

			public long Next()
			{
				do
				{
					Previous = MathM.Mod(Previous * factor, 2147483647);
				} while (Previous % mult != 0);
				return Previous;
			}
		}

		[Part(1)]
		public object Part1(string input)
		{
			string[] lines = input.Lines();
			Generator a = new Generator(long.Parse(lines[0].Words()[^1]), 16807, 1);
			Generator b = new Generator(long.Parse(lines[1].Words()[^1]), 48271, 1);

			int count = 0;
			for (int i = 0; i < 40000000; i++)
			{
				long aVal = a.Next() & 0xffff;
				long bVal = b.Next() & 0xffff;
				if (aVal == bVal)
				{
					count++;
				}
			}

			return count;
		}

		[Part(2)]
		public object Part2(string input)
		{
			string[] lines = input.Lines();
			Generator a = new Generator(long.Parse(lines[0].Words()[^1]), 16807, 4);
			Generator b = new Generator(long.Parse(lines[1].Words()[^1]), 48271, 8);

			int count = 0;
			for (int i = 0; i < 5000000; i++)
			{
				long aVal = a.Next() & 0xffff;
				long bVal = b.Next() & 0xffff;
				if (aVal == bVal)
				{
					count++;
				}
			}

			return count;
		}
	}
}
