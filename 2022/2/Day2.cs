using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2022;

[Day(2)]
public class Day2
{
	[Part(1)]
	public object Part1(string input) =>
		input.Lines()
			.Extract<(char, char)>(@"(.) (.)")
			.Select(ScorePlain)
			.Sum();

	private long ScorePlain((char, char) p)
	{
		long elf = (long)(p.Item1 - 'A');
		long me = (long)(p.Item2 - 'X');

		long win = 0;
		if (elf == me)
		{
			win = 3;
		}
		else if (MathM.Mod(elf + 1, 3) == me)
		{
			win = 6;
		}

		return 1 + me + win;
	}

	[Part(2)]
	public object Part2(string input) =>
		input.Lines()
			.Extract<(char, char)>(@"(.) (.)")
			.Select(ScoreEncrypted)
			.Sum();

	private long ScoreEncrypted((char, char) p)
	{
		long elf = (long)(p.Item1 - 'A');
		long win = (long)(p.Item2 - 'X') * 3;

		long me = elf;
		if (win == 0)
		{
			me = MathM.Mod(elf - 1, 3);
		}
		else if (win == 6)
		{
			me = MathM.Mod(elf + 1, 3);
		}

		return 1 + me + win;
	}
}
