using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;

namespace AoC.Library
{
	public static class IEnumerableUtils
	{
		/// <summary>
		/// Turns a sequence into a series of sliding window subsets of size 2.
		/// </summary>
		public static IEnumerable<(T, T)> Window<T>(this IEnumerable<T> source) =>
			source.Zip(source.Skip(1), static (a, b) => (a, b));

		/// <summary>
		/// Turns a sequence into a series of sliding window subsets of size 3.
		/// </summary>
		public static IEnumerable<(T, T, T)> Window3<T>(this IEnumerable<T> source) =>
			source.Zip(source.Skip(1), static (a, b) => (a, b))
				.Zip(source.Skip(2), static (p, c) => (p.a, p.b, c));

		/// <summary>
		/// Turns a sequence into a series of sliding window subsets of size 4.
		/// </summary>
		public static IEnumerable<(T, T, T, T)> Window4<T>(this IEnumerable<T> source) =>
			source.Zip(source.Skip(1), static (a, b) => (a, b))
				.Zip(source.Skip(2), static (p, c) => (p.a, p.b, c))
				.Zip(source.Skip(3), static (p, d) => (p.a, p.b, p.c, d));

		/// <summary>
		/// Computes the product of a sequence of <see cref="long"/> values.
		/// </summary>
		public static long Product(this IEnumerable<long> source) =>
			source.Aggregate(1L, static (p, n) => p * n);

		/// <summary>
		/// Computes the product of the sequence of <see cref="long"/> values that are obtained by
		/// invoking a transform function on each element of the input sequence.
		/// </summary>
		public static long Product<T>(this IEnumerable<T> source, Func<T, long> selector) =>
			source.Select(selector).Product();


		/// <summary>
		/// Like <see cref="Enumerable.GroupBy{TSource, TKey}(IEnumerable{TSource}, Func{TSource, TKey}, IEqualityComparer{TKey}?)"/>,
		/// but only groups adjacent elements.
		/// </summary>
		public static IEnumerable<IGrouping<TKey, TSource>> AdjacentGroupBy<TSource, TKey>(
			this IEnumerable<TSource> source,
			Func<TSource, TKey> keySelector,
			IEqualityComparer<TKey>? comparer = null)
		{
			comparer ??= EqualityComparer<TKey>.Default;

			Grouping<TKey, TSource>? currentGroup = null;
			foreach (var element in source)
			{
				var elementKey = keySelector(element);
				if (currentGroup is not null && comparer.Equals(currentGroup.Key, elementKey))
				{
					currentGroup.Add(element);
				}
				else
				{
					if (currentGroup is not null)
					{
						yield return currentGroup;
					}

					currentGroup = new Grouping<TKey, TSource>(elementKey);
					currentGroup.Add(element);
				}
			}

			if (currentGroup is not null)
			{
				yield return currentGroup;
			}
		}

		private class Grouping<TKey, TElement> : IGrouping<TKey, TElement>
		{
			public TKey Key { get; }
			private ICollection<TElement> elements = new List<TElement>();

			public Grouping(TKey key) =>
				(Key) = (key);

			public void Add(TElement element) =>
				elements.Add(element);

			public IEnumerator<TElement> GetEnumerator() =>
				elements.GetEnumerator();
			IEnumerator IEnumerable.GetEnumerator() =>
				GetEnumerator();
		}

		/// <summary>
		/// Returns a sequence where each value is paired with its index.
		/// </summary>
		public static IEnumerable<KeyValuePair<int, T>> Enumerate<T>(this IEnumerable<T> source) =>
			source.Select(static (e, i) => new KeyValuePair<int, T>(i, e));

		/// <summary>
		/// Selects over a sequence and discards any null resulting element.
		/// </summary>
		public static IEnumerable<U> SelectWhere<T, U>(this IEnumerable<T> source, Func<T, U?> selector) where U : struct =>
			source.Select(selector)
				.Where(static t => t is not null)
				.Cast<U>();

		/// <summary>
		/// Selects over a sequence and discards any null resulting element.
		/// </summary>
		public static IEnumerable<U> SelectWhere<T, U>(this IEnumerable<T> source, Func<T, U?> selector) where U : class =>
			source.Select(selector)
				.Where(static t => t is not null)
				.Cast<U>();

		/// <summary>
		/// Returns a transposed enumerable enumerable of the <paramref name="source"/>. A jagged source does not
		/// result in any gaps in the result, instead the result is jagged as well.
		/// </summary>
		public static IEnumerable<IEnumerable<T>> Transpose<T>(this IEnumerable<IEnumerable<T>> source)
		{
			var enumerators = source.Select(e => e.GetEnumerator()).ToArray();
			while (true)
			{
				var row = new List<T>();
				foreach (var enumerator in enumerators)
				{
					if (enumerator.MoveNext())
					{
						row.Add(enumerator.Current);
					}
				}

				if (row.Count > 0)
				{
					yield return row;
				}
				else
				{
					break;
				}
			}
		}

		/// <summary>
		/// Converts a sequence of bytes to a "human readable" hex-string.
		/// </summary>
		public static string ToHexString(this IEnumerable<byte> source)
		{
			if (source.TryGetNonEnumeratedCount(out int count))
			{
				return String.Create(count * 2, source, FillSpanWithHexFromEnumerable);
			}
			else
			{
				var array = source.ToArray();
				return String.Create(array.Length * 2, array, FillSpanWithHexFromArray);
			}

			static void FillSpanWithHexFromArray(Span<char> str, byte[] bytes)
			{
				for (int i = 0; i < bytes.Length; i++)
				{
					str[i * 2] = IntToHex(bytes[i] >> 4);
					str[i * 2 + 1] = IntToHex(bytes[i] & 0x0f);
				}
			}

			static void FillSpanWithHexFromEnumerable(Span<char> str, IEnumerable<byte> source)
			{
				int i = 0;
				foreach (byte b in source)
				{
					str[i * 2] = IntToHex(b >> 4);
					str[i * 2 + 1] = IntToHex(b & 0x0f);
					i++;
				}
			}
		}
		/// <summary>
		/// Converts a span of bytes to a "human readable" hex-string.
		/// </summary>
		public static string ToHexString(this Span<byte> source)
		{
			char[] chars = new char[source.Length * 2];
			for (int i = 0; i < source.Length; i++)
			{
				chars[i * 2] = IntToHex(source[i] >> 4);
				chars[i * 2 + 1] = IntToHex(source[i] & 0x0f);
			}
			return new String(chars);
		}
		private static char IntToHex(int n) =>
			(char)(n < 10 ? (n + '0') : (n - 10 + 'a'));
	}
}
