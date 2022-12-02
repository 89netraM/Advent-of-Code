using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(2)]
public class Day2
{
	[Part(1)]
	public object Part1(string input) =>
		Score(input, ScorePlain);

	[Part(2)]
	public object Part2(string input) =>
		Score(input, ScoreEncrypted);

	private long Score(string input, Func<char, char, long> scoringFunction) =>
		input.Lines()
			.Sum(l => scoringFunction(l[0], l[2]));

	private long ScorePlain(char elfC, char meC)
	{
		long elf = (long)(elfC - 'A');
		long me = (long)(meC - 'X');
		long win = MathM.Mod(me - elf + 1, 3) * 3;
		return 1 + me + win;
	}

	private long ScoreEncrypted(char elfC, char winC)
	{
		long elf = (long)(elfC - 'A');
		long win = (long)(winC - 'X') * 3;
		long me = MathM.Mod(elf + (win / 3 - 1), 3);
		return 1 + me + win;
	}
}
