using System;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;
using RegExtract;

namespace AoC.Year2021
{
	[Day(21)]
	public class Day21
	{
		[Part(1)]
		public object Part1(string input)
		{
			var starts = input.Lines().Extract<long>(@"^Player \d starting position: (\d+)$").ToArray();
			var rollCount = 0L;
			var p1 = starts[0] - 1L;
			var p1Score = 0L;
			var p2 = starts[1] - 1L;
			var p2Score = 0L;
			var die = 99L;

			while (p2Score < 1000L)
			{
				var roll = Roll() + Roll() + Roll();
				p1 = MathM.Mod(p1 + roll, 10L);
				p1Score += p1 + 1;

				if (p1Score >= 1000L)
				{
					break;
				}

				roll = Roll() + Roll() + Roll();
				p2 = MathM.Mod(p2 + roll, 10L);
				p2Score += p2 + 1;
			}

			return Math.Min(p1Score, p2Score) * rollCount;

			long Roll()
			{
				rollCount++;
				die = MathM.Mod(die + 1, 100L);
				return die + 1;
			}
		}

		[Part(2)]
		public object Part2(string input)
		{
			var starts = input.Lines().Extract<long>(@"^Player \d starting position: (\d+)$").ToArray();
			var (p1Wins, p2Wins) = CountWins2(new(starts[0] - 1L, 0L, starts[1] - 1L, 0L));
			return Math.Max(p1Wins, p2Wins);
		}

		public static Func<Play, (long, long)> CountWins2 = Memoize<Play, (long, long)>(static play =>
		{
			const long winScore = 21L;
			if (play.p1Score >= winScore)
			{
				return (1, 0);
			}
			if (play.p2Score >= winScore)
			{
				return (0, 1);
			}
			var result = (p1Wins: 0L, p2Wins: 0L);
			for (long i = 1; i <= 3; i++)
			{
				for (long j = 1; j <= 3; j++)
				{
					for (long k = 1; k <= 3; k++)
					{
						var p1New = MathM.Mod(play.p1 + i + j + k, 10L);
						var (p1Wins, p2Wins) = CountWins2(new(play.p2, play.p2Score, p1New, play.p1Score + 1L + p1New));
						result.p1Wins += p2Wins;
						result.p2Wins += p1Wins;
					}
				}
			}
			return result;
		});

		public record Play(long p1, long p1Score, long p2, long p2Score);
	}
}
