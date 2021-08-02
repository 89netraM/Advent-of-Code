using System;
using System.Numerics;
using System.Linq;
using FsCheck;

namespace AoC.Library.Test
{
	public static class Utils
	{
		public static Arbitrary<long> PositiveLong { get; } =
			Arb.From<long>().MapFilter(static l => Math.Abs(l), static l => l >= 0);

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
}
