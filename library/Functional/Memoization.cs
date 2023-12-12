using System;
using System.Collections.Generic;

namespace AoC.Library
{
	internal class Memoization<TA, TR> where TA : notnull
	{
		private readonly IDictionary<TA, TR?> cache;
		private readonly Func<TA, TR?> func;

		public Memoization(Func<TA, TR?> func) =>
			(this.func, cache) = (func, new Dictionary<TA, TR?>());
		public Memoization(Func<TA, TR?> func, IEqualityComparer<TA> equalityComparer) =>
			(this.func, cache) = (func, new Dictionary<TA, TR?>());

		public TR? Invoke(TA arg)
		{
			TR? result;
			if (cache.TryGetValue(arg, out result))
			{
				return result;
			}
			else
			{
				result = func(arg);
				cache.Add(arg, result);
				return result;
			}
		}

		public static implicit operator Func<TA, TR?>(Memoization<TA, TR> memo) =>
			memo.Invoke;
	}
}
