using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2015
{
	[Day(16)]
	public class Day16
	{
		private static readonly IReadOnlyDictionary<string, long> Sender = new Dictionary<string, long>
		{
			["children"] = 3,
			["cats"] = 7,
			["samoyeds"] = 2,
			["pomeranians"] = 3,
			["akitas"] = 0,
			["vizslas"] = 0,
			["goldfish"] = 5,
			["trees"] = 3,
			["cars"] = 2,
			["perfumes"] = 1,
		};
		private static readonly IReadOnlyDictionary<string, Func<long, bool>> RealSender = new Dictionary<string, Func<long, bool>>
		{
			["children"] = l => l == 3,
			["cats"] = l => l > 7,
			["samoyeds"] = l => l == 2,
			["pomeranians"] = l => l < 3,
			["akitas"] = l => l == 0,
			["vizslas"] = l => l == 0,
			["goldfish"] = l => l < 5,
			["trees"] = l => l > 3,
			["cars"] = l => l == 2,
			["perfumes"] = l => l == 1,
		};

		[Part(1)]
		public object Part1(string input) =>
			input.Lines()
				.Select((l, i) => (i: i + 1,
					d: l.Extract<List<string>>(@"(?:(\w+: \d+)(?:, )?)+")
						.Select(p => p.Split(':'))
						.ToDictionary(p => p[0], p => Int32.Parse(p[1].Trim()))))
				.Where(p => p.d.All(kvp => Sender[kvp.Key] == kvp.Value))
				.Single()
				.i;

		[Part(2)]
		public object Part2(string input) =>
			input.Lines()
				.Select((l, i) => (i: i + 1,
					d: l.Extract<List<string>>(@"(?:(\w+: \d+)(?:, )?)+")
						.Select(p => p.Split(':'))
						.ToDictionary(p => p[0], p => Int32.Parse(p[1].Trim()))))
				.Where(p => p.d.All(kvp => RealSender[kvp.Key](kvp.Value)))
				.Single()
				.i;
	}
}
