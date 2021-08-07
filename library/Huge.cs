using System;
using System.Collections.Generic;
using System.Linq;

namespace AoC.Library
{
	[Flags]
	public enum HugeSearchPattern
	{
		Period,
		ArithmeticProgression,
		GeometricProgression,
		Progressions = ArithmeticProgression | GeometricProgression,
		All = Period | ArithmeticProgression | GeometricProgression,
	}

	public static class Huge
	{
		private const long ProgressionTestLength = 5L;

		public static long TakeSteps(long steps, Func<long, long> update, HugeSearchPattern pattern = HugeSearchPattern.All)
		{
			IDictionary<long, long> seen = new Dictionary<long, long>();
			Queue<long> previous = new Queue<long>();

			long? current = null;
			for (long i = 0L; i < steps; i++)
			{
				current = update(i);

				// Period
				if (pattern.HasFlag(HugeSearchPattern.Period))
				{
					if (seen.TryGetValue(current.Value, out long stepsToPrevious))
					{
						long oldI = i;
						i = steps - ((steps - stepsToPrevious) % (i - stepsToPrevious));
						Console.WriteLine($"Found a period from step {stepsToPrevious} to step {oldI}, and used it to skip to step {i}.");
					}
					else
					{
						seen.Add(current.Value, i);
					}
				}

				// Progressions
				if (pattern.HasFlag(HugeSearchPattern.Progressions))
				{
					previous.Enqueue(current.Value);
					if (previous.Count > ProgressionTestLength)
					{
						previous.Dequeue();
					}
					if (previous.Count == ProgressionTestLength)
					{
						long begining = previous.First();
						long beginingIndex = i - (ProgressionTestLength - 1L);

						// Arithmetic progression
						if (pattern.HasFlag(HugeSearchPattern.ArithmeticProgression))
						{
							IEnumerable<long> diffs = previous.Zip(previous.Skip(1), static (a, b) => b - a).Distinct();
							if (diffs.Count() == 1)
							{
								long diff = diffs.Single();
								Console.WriteLine($"Found an arithmetic progression with difference {diff} after step {beginingIndex} (value {begining}).");
								return begining + (steps - beginingIndex - 1L) * diff;
							}
						}

						// Geometric progression
						if (pattern.HasFlag(HugeSearchPattern.GeometricProgression))
						{
							IEnumerable<double> ratios = previous.Zip(previous.Skip(1), static (a, b) => b / (double)a).Distinct();
							if (ratios.Count() == 1)
							{
								double ratio = ratios.Single();
								Console.WriteLine($"Found a geometric progression with ratio {ratio} after step {beginingIndex} (value {begining}).");
								return (long)(begining * Math.Pow(ratio, (steps - beginingIndex - 1L)));
							}
						}
					}
				}
			}

			return current ?? throw new Exception("Took zero steps and never calculated the result.");
		}
	}
}
