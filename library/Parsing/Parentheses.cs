using System;
using System.Collections.Generic;
using System.Linq;

namespace AoC.Library
{
	public static partial class Parsing
	{
		private const char DefaultLeftParantheses = '(';
		private const char DefaultRightParantheses = ')';
		/// <summary>
		/// Finds and outputs the contents of all top-level parentheses in the
		/// given string.
		/// </summary>
		public static IEnumerable<string> ParseParentheses(string input, char leftParantheses = DefaultLeftParantheses, char rightParantheses = DefaultRightParantheses) =>
			ParseParentheses(input.AsMemory(), leftParantheses, rightParantheses)
				.Select(static m => new String(m.ToArray()));
		/// <summary>
		/// Finds and outputs the contents of all top-level parentheses in the
		/// given memory section.
		/// </summary>
		public static IEnumerable<ReadOnlyMemory<char>> ParseParentheses(ReadOnlyMemory<char> input, char leftParantheses = DefaultLeftParantheses, char rightParantheses = DefaultRightParantheses)
		{
			IList<ReadOnlyMemory<char>> result = new List<ReadOnlyMemory<char>>();

			int level = 0;
			int entryIndex = -1;
			for (int i = 0; i < input.Length; i++)
			{
				if (input.Span[i] == leftParantheses)
				{
					level++;
					if (level == 1)
					{
						entryIndex = i;
					}
				}
				else if (input.Span[i] == rightParantheses)
				{
					level--;
					if (level == 0)
					{
						result.Add(input.Slice(entryIndex + 1, i - entryIndex - 1));
					}
				}
			}

			return result;
		}

		/// <summary>
		/// Finds and outputs the contents of all parentheses, regardless of
		/// level, in the given string.
		/// </summary>
		public static IEnumerable<string> ParseDeepParentheses(string input, char leftParantheses = DefaultLeftParantheses, char rightParantheses = DefaultRightParantheses) =>
			ParseDeepParentheses(input.AsMemory(), leftParantheses, rightParantheses)
				.Select(static m => new String(m.ToArray()));
		/// <summary>
		/// Finds and outputs the contents of all parentheses, regardless of
		/// level, in the given memory section.
		/// </summary>
		public static IEnumerable<ReadOnlyMemory<char>> ParseDeepParentheses(ReadOnlyMemory<char> input, char leftParantheses = DefaultLeftParantheses, char rightParantheses = DefaultRightParantheses) =>
			ParseParentheses(input, leftParantheses, rightParantheses)
				.SelectMany(m => Enumerable.Repeat(m, 1).Concat(ParseDeepParentheses(m, leftParantheses, rightParantheses)));
	}
}
