using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using FsCheck;

namespace AoC.Library.Test
{
	public static class Utils
	{
		public static Arbitrary<int> PositiveInt { get; } =
			Arb.From<int>().MapFilter(static l => Math.Abs(l), static l => l >= 0);
		public static Arbitrary<long> PositiveLong { get; } =
			Arb.From<long>().MapFilter(static l => Math.Abs(l), static l => l >= 0);

		public static Arbitrary<int> NonZeroInt { get; } =
			PositiveInt.Filter(static l => l != 0);
		public static Arbitrary<long> NonZeroLong { get; } =
			PositiveLong.Filter(static l => l != 0);

		public static Arbitrary<T> NotNull<T>() =>
			Arb.From<T>().Filter(static t => t is not null);

		public static long DelannoyNumber(long m, long n)
		{
			long result = 0L;
			for (long k = 0; k <= Math.Min(m, n); k++)
			{
				result += BinomialCoefficient(m, k) * BinomialCoefficient(n, k) * (long)Math.Pow(2, k);
			}
			return result;
		}
		public static long BinomialCoefficient(long n, long k)
		{
			long res = 1;
			if (k > n - k)
			{
				k = n - k;
			}
			for (long i = 0; i < k; i++)
			{
				res *= (n - i);
				res /= (i + 1);
			}
			return res;
		}
	}

	public class SequenceEqualityComparer<T> : IEqualityComparer<IEnumerable<T>>
	{
		private readonly IEqualityComparer<T> equalityComparer;

		public SequenceEqualityComparer() =>
			equalityComparer = EqualityComparer<T>.Default;
		public SequenceEqualityComparer(IEqualityComparer<T> equalityComparer) =>
			this.equalityComparer = equalityComparer;

		public bool Equals(IEnumerable<T>? x, IEnumerable<T>? y) =>
			(x is null && y is null) || (x is not null && y is not null && x.SequenceEqual(y));

		public int GetHashCode([DisallowNull] IEnumerable<T> obj)
		{
			var hc = new HashCode();
			foreach (var t in obj)
			{
				hc.Add(t);
			}
			return hc.ToHashCode();
		}
	}
}
