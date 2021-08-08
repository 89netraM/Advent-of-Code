using System;

namespace AoC.Library
{
	public static class StringUtils
	{
		private static readonly string[] lineSeparators = new[] { "\r\n", "\n" };
		private static readonly string[] wordSeparators = new[] { " ", "\t", "\r\n", "\n" };

		public static string[] Lines(this string s) =>
			s.Split(lineSeparators, StringSplitOptions.None);

		public static string[] Words(this string s) =>
			s.Split(wordSeparators, StringSplitOptions.RemoveEmptyEntries);
	}
}
