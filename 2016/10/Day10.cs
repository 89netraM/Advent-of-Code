using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2016;

[Day(10)]
public class Day10
{
	[Part(1)]
	public object Part1(string input)
	{
		var instructions = input.Lines().ToArray();

		var bots = new Dictionary<long, SortedList<long, long>>();
		foreach (var ins in instructions)
		{
			if (ins.StartsWith("value"))
			{
				var (val, bot) = ins.Extract<(long, long)>(@"value (\d+) goes to bot (\d+)");
				bots.AddOrModify(bot, new SortedList<long, long> { {val, val} }, l => l.Add(val, val));
			}
		}

		var updated = false;
		do
		{
			updated = false;

			foreach (var ins in instructions)
			{
				if (ins.StartsWith("bot"))
				{
					var (fromBot, lowKind, lowTarget, highKind, highTarget) = ins.Extract<(long, string, long, string, long)>(@"bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)");
					if (bots.TryGetValue(fromBot, out var from) && from.Count >= 2)
					{
						var lowVal = from.GetValueAtIndex(0);
						var highVal = from.GetValueAtIndex(from.Count - 1);
						from.RemoveAt(0);
						from.RemoveAt(from.Count - 1);

						if (lowVal == 17 && highVal == 61)
						{
							return fromBot;
						}

						if (lowKind == "bot")
						{
							bots.AddOrModify(lowTarget, new SortedList<long, long> { {lowVal, lowVal} }, l => l.Add(lowVal, lowVal));
						}
						if (highKind == "bot")
						{
							bots.AddOrModify(highTarget, new SortedList<long, long> { {highVal, highVal} }, l => l.Add(highVal, highVal));
						}

						updated = true;
						break;
					}
				}
			}
		} while (updated);

		return null;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var instructions = input.Lines().ToArray();

		var bots = new Dictionary<long, SortedList<long, long>>();
		foreach (var ins in instructions)
		{
			if (ins.StartsWith("value"))
			{
				var (val, bot) = ins.Extract<(long, long)>(@"value (\d+) goes to bot (\d+)");
				bots.AddOrModify(bot, new SortedList<long, long> { {val, val} }, l => l.Add(val, val));
			}
		}

		var outputs = new Dictionary<long, SortedList<long, long>>
		{
			[0] = new SortedList<long, long>(),
			[1] = new SortedList<long, long>(),
			[2] = new SortedList<long, long>(),
		};

		var updated = false;
		do
		{
			updated = false;

			foreach (var ins in instructions)
			{
				if (ins.StartsWith("bot"))
				{
					var (fromBot, lowKind, lowTarget, highKind, highTarget) = ins.Extract<(long, string, long, string, long)>(@"bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)");
					if (bots.TryGetValue(fromBot, out var from) && from.Count >= 2)
					{
						var lowVal = from.GetValueAtIndex(0);
						var highVal = from.GetValueAtIndex(from.Count - 1);
						from.RemoveAt(0);
						from.RemoveAt(from.Count - 1);

						if (lowKind == "bot")
						{
							bots.AddOrModify(lowTarget, new SortedList<long, long> { {lowVal, lowVal} }, l => l.Add(lowVal, lowVal));
						}
						else
						{
							outputs.AddOrModify(lowTarget, new SortedList<long, long> { { lowVal, lowVal } }, l => l.Add(lowVal, lowVal));
							if (outputs.TryGetValue(0, out var a) && a.Count > 0 && outputs.TryGetValue(1, out var b) && b.Count > 0 && outputs.TryGetValue(2, out var c) && c.Count > 0)
							{
								return a.GetValueAtIndex(0) * b.GetValueAtIndex(0) * c.GetValueAtIndex(0);
							}
						}
						if (highKind == "bot")
						{
							bots.AddOrModify(highTarget, new SortedList<long, long> { {highVal, highVal} }, l => l.Add(highVal, highVal));
						}
						else
						{
							outputs.AddOrModify(highTarget, new SortedList<long, long> { { highVal, highVal } }, l => l.Add(highVal, highVal));
							if (outputs.TryGetValue(0, out var a) && a.Count > 0 && outputs.TryGetValue(1, out var b) && b.Count > 0 && outputs.TryGetValue(2, out var c) && c.Count > 0)
							{
								return a.GetValueAtIndex(0) * b.GetValueAtIndex(0) * c.GetValueAtIndex(0);
							}
						}

						updated = true;
						break;
					}
				}
			}
		} while (updated);

		return null;
	}
}
