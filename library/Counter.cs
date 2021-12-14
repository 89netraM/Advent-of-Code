using System;
using System.Collections.Generic;
using System.Linq;
using static AoC.Library.Functional;

namespace AoC.Library
{
	public static class Counter
	{
		/// <summary>
		/// Counts the number of occurrences of each element in the source sequence and stores the result in a counter (dictionary).
		/// </summary>
		public static Dictionary<TElement, long> ToCounter<TElement>(this IEnumerable<TElement> source) where TElement : notnull =>
			source.GroupBy(Id).ToDictionary(static g => g.Key, Enumerable.LongCount);

		/// <summary>
		/// Adds <paramref name="amount"/> to <paramref name="element"/>.
		/// </summary>
		public static void Increase<TElement>(this IDictionary<TElement, long> source, TElement element, long amount = 1L) where TElement : notnull =>
			source[element] = (source.TryGetValue(element, out long v) ? v : 0L) + amount;
		/// <summary>
		/// Adds the elements of the other counter to this counter.
		/// </summary>
		public static void Increase<TElement>(this IDictionary<TElement, long> source, IDictionary<TElement, long> other) where TElement : notnull
		{
			foreach (var kvp in other)
			{
				source.Increase(kvp.Key, kvp.Value);
			}
		}
		/// <summary>
		/// Adds the elements of the other collection once per occurenc to this counter.
		/// </summary>
		public static void Increase<TElement>(this IDictionary<TElement, long> source, IEnumerable<TElement> other) where TElement : notnull
		{
			foreach (var element in other)
			{
				source.Increase(element);
			}
		}

		/// <summary>
		/// Subtracts <paramref name="amount"/> from <paramref name="element"/>.
		/// </summary>
		/// <remarks>
		/// Counts can go into the negative.
		/// </remarks>
		public static void Decrease<TElement>(this IDictionary<TElement, long> source, TElement element, long amount = 1L) where TElement : notnull =>
			source.Increase(element, -amount);
		/// <summary>
		/// Subtracts the elements of the other counter from this counter.
		/// </summary>
		/// <remarks>
		/// Counts can go into the negative.
		/// </remarks>
		public static void Decrease<TElement>(this IDictionary<TElement, long> source, IDictionary<TElement, long> other) where TElement : notnull
		{
			foreach (var kvp in other)
			{
				source.Decrease(kvp.Key, kvp.Value);
			}
		}
		/// <summary>
		/// Subtracts the elements of the other collection once per occurenc from this counter.
		/// </summary>
		/// <remarks>
		/// Counts can go into the negative.
		/// </remarks>
		public static void Decrease<TElement>(this IDictionary<TElement, long> source, IEnumerable<TElement> other) where TElement : notnull
		{
			foreach (var element in other)
			{
				source.Decrease(element);
			}
		}

		/// <summary>
		/// Return an enumerable over elements, repeating each as many times as its count.
		/// </summary>
		/// <remarks>
		/// 	<list type="bullet">
		/// 		<item>Elements are returned in no particular order.</item>
		/// 		<item>If an element’s count is less than one, it will be ignored.</item>
		/// 	</list>
		/// </remarks>
		public static IEnumerable<TElement> Elements<TElement>(this IDictionary<TElement, long> source) where TElement : notnull =>
			source.SelectMany(static kvp => Enumerable.Repeat(kvp.Key, Math.Max(0, (int)kvp.Value)));

		/// <summary>
		/// Return an enumerable of the elements and their counts from the most common to the least.
		/// </summary>
		/// <remarks>
		/// Elements with equal counts are returned in no particular order.
		/// </remarks>
		public static IEnumerable<(TElement element, long count)> MostCommon<TElement>(this IDictionary<TElement, long> source) where TElement : notnull =>
			source.MostCommon(source.Count);
		/// <summary>
		/// Return an enumerable of the <paramref name="n"/> most common elements and their counts from the most common to the least.
		/// </summary>
		/// <remarks>
		/// Elements with equal counts are returned in no particular order.
		/// </remarks>
		public static IEnumerable<(TElement element, long count)> MostCommon<TElement>(this IDictionary<TElement, long> source, int n) where TElement : notnull =>
			source.OrderByDescending(static kvp => kvp.Value).Select(static kvp => (kvp.Key, kvp.Value)).Take(n);

		/// <summary>
		/// Compute the sum of the counts.
		/// </summary>
		public static long Total<TElement>(this IDictionary<TElement, long> source) where TElement : notnull =>
			source.Sum(static kvp => kvp.Value);
	}
}
