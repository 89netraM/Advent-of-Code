using System;
using System.Globalization;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(8)]
	public class Day8
	{
		[Part(1)]
		public long Part1(string input) =>
			input.Lines()
				.Select(l => l.Trim())
				.Select(l => l.Length - Eval(l).Length)
				.Sum();

		private string Eval(string input)
		{
			var regex = new Regex(@"\\\\|\\""|\\x([\da-f]{2})");
			return regex.Replace(input[1..^1], Replacer);

			static string Replacer(Match match) =>
				match.Groups[0].Value switch
				{
					@"\\" => @"\",
					@"\""" => @"""",
					_ => Char.ConvertFromUtf32(Int32.Parse(match.Groups[1].Value, NumberStyles.HexNumber)),
				};
		}

		[Part(2)]
		public long Part2(string input) =>
			input.Lines()
				.Select(l => l.Trim())
				.Select(l => Escape(l).Length - l.Length)
				.Sum();

		private string Escape(string input)
		{
			var regex = new Regex(@"\\|""");
			return $@"""{regex.Replace(input, Replacer)}""";

			static string Replacer(Match match) =>
				match.Groups[0].Value switch
				{
					@"\" => @"\\",
					@"""" => @"\""",
					_ => throw new Exception("Impossible!"),
				};
		}
	}
}
