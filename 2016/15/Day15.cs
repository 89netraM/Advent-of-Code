using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(15)]
public class Day15
{
	[Part(1)]
	public object Part1(string input)
	{
		var disks = input.Lines()
			.Extract<Disk>(@"Disc #(?:\d+) has (\d+) positions; at time=0, it is at position (\d+).")
			.ToArray();

		for (long dropTime = 0; true; dropTime++)
		{
			for (int i = 0; i < disks.Length; i++)
			{
				if (disks[i].PositionAt(dropTime + i + 1) != 0)
				{
					goto next;
				}
			}
			return dropTime;
			next:;
		}
	}

	[Part(2)]
	public object Part2(string input)
	{
		var disks = input.Lines()
			.Extract<Disk>(@"Disc #(?:\d+) has (\d+) positions; at time=0, it is at position (\d+).")
			.Append(new Disk(11, 0))
			.ToArray();

		for (long dropTime = 0; true; dropTime++)
		{
			for (int i = 0; i < disks.Length; i++)
			{
				if (disks[i].PositionAt(dropTime + i + 1) != 0)
				{
					goto next;
				}
			}
			return dropTime;
			next:;
		}
	}

	private record Disk(long PositionCount, long StartPosition)
	{
		public long PositionAt(long time) =>
			MathM.Mod(StartPosition + time, PositionCount);
	}
}
