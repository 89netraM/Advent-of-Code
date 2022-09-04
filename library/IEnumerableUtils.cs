using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

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
	}
}
