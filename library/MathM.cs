using System;
using System.Collections.Generic;
using System.Linq;

namespace AoC.Library
{
	public static class MathM
	{
		public static int Mod(int a, int b) =>
			(a % b + b) % b;
		public static long Mod(long a, long b) =>
			(a % b + b) % b;

		/// <summary>
		/// Finds the two numbers greatest common divisor.
		/// </summary>
		public static long Gcd(long a, long b)
		{
			while (b != 0L)
			{
				(a, b) = (b, a % b);
			}
			return Math.Abs(a);
		}

		/// <summary>
		/// Finds the greatest common divisor among the numbers.
		/// </summary>
		public static long Gcd(params long[] numbers) =>
			numbers.Gcd();
		/// <summary>
		/// Finds the greatest common divisor among the numbers.
		/// </summary>
		public static long Gcd(this IEnumerable<long> numbers) =>
			numbers.Aggregate(Gcd);

		/// <summary>
		/// Finds the two numbers least common multiple.
		/// </summary>
		public static long Lcm(long a, long b) =>
			Math.Abs(a * b) / Gcd(a, b);

		/// <summary>
		/// Finds the least common multiple among the numbers.
		/// </summary>
		public static long Lcm(params long[] numbers) =>
			numbers.Lcm();
		/// <summary>
		/// Finds the least common multiple among the numbers.
		/// </summary>
		public static long Lcm(this IEnumerable<long> numbers) =>
			numbers.Aggregate(Lcm);
	}
}
