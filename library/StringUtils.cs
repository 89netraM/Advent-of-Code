using System;
using System.Collections.Generic;
using System.Linq;
using static AoC.Library.Functional;

namespace AoC.Library
{
	public static class StringUtils
	{
		private static readonly string[] paragraphSeparators = new[] { "\r\n\r\n", "\n\n" };
		private static readonly string[] lineSeparators = new[] { "\r\n", "\n" };
		private static readonly string[] wordSeparators = new[] { " ", "\t", "\r\n", "\n" };

		public static string[] Paragraphs(this string s) =>
			s.Split(paragraphSeparators, StringSplitOptions.None);

		public static string[] Lines(this string s) =>
			s.Split(lineSeparators, StringSplitOptions.None);

		public static string[] Words(this string s) =>
			s.Split(wordSeparators, StringSplitOptions.RemoveEmptyEntries);

		/// <summary>
		/// Transforms a string into a <see cref="Vector2"/> map of characters.
		/// </summary>
		public static Dictionary<Vector2, char> ToMap(this string s) =>
			s.ToMap(Id);
		/// <summary>
		/// Transforms a string into a <see cref="Vector2"/> map of longs, assuming the characters are ASCII numbers.
		/// </summary>
		public static Dictionary<Vector2, long> ToMapLong(this string s) =>
			s.ToMap(static c => (long)(c - '0'));
		/// <summary>
		/// Transforms a string into a <see cref="Vector2"/> map where values are built by the <paramref name="transformer"/>.
		/// </summary>
		public static Dictionary<Vector2, T> ToMap<T>(this string s, Func<char, T> transformer) =>
			s.Lines()
				.SelectMany((l, y) => l.Select((c, x) => (x, y, c)))
				.ToDictionary(t => new Vector2(t.x, t.y), t => transformer(t.c));
	}
}
