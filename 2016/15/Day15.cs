using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(15)]
public class Day15
{
	[Part(1)]
	public long Part1(string input) =>
		ParseDisks(input)
			.Let(FindFirstDropTime);

	[Part(2)]
	public long Part2(string input) =>
		ParseDisks(input)
			.Append(new Disk(11, 0))
			.ToArray()
			.Let(FindFirstDropTime);

	private IEnumerable<Disk> ParseDisks(string input) =>
		input.Lines()
			.Extract<Disk>(@"Disc #(?:\d+) has (\d+) positions; at time=0, it is at position (\d+).");

	private long FindFirstDropTime(IEnumerable<Disk> disks)
	{
		for (long time = 0; true; time++)
		{
			if (CanDropAtTime(disks, time))
			{
				return time;
			}
		}
	}

	private bool CanDropAtTime(IEnumerable<Disk> disks, long time) =>
		disks.Enumerate()
			.All(kvp => kvp.Value.PositionAt(time + kvp.Key + 1) == 0);

	private record Disk(long PositionCount, long StartPosition)
	{
		public long PositionAt(long time) =>
			MathM.Mod(StartPosition + time, PositionCount);
	}
}
