using System;

namespace AoC.Library
{
	public static class Functional
	{
		public static T Id<T>(T t) => t;

		public static Func<T, bool> Not<T>(Func<T, bool> f) =>
			t => !f(t);

		public static Func<TA1, TA2, TR> Curry<TA1, TA2, TR>(Func<(TA1, TA2), TR> f) =>
			(a1, a2) => f((a1, a2));
		public static Func<TA1, TA2, TA3, TR> Curry<TA1, TA2, TA3, TR>(Func<(TA1, TA2, TA3), TR> f) =>
			(a1, a2, a3) => f((a1, a2, a3));
		public static Func<(TA1, TA2), TR> Uncurry<TA1, TA2, TR>(Func<TA1, TA2, TR> f) =>
			p => f(p.Item1, p.Item2);
		public static Func<(TA1, TA2, TA3), TR> Uncurry<TA1, TA2, TA3, TR>(Func<TA1, TA2, TA3, TR> f) =>
			p => f(p.Item1, p.Item2, p.Item3);

		public static Func<TA, TR> Memoize<TA, TR>(Func<TA, TR> f) where TA : notnull =>
			new Memoization<TA, TR>(f);
	}
}
