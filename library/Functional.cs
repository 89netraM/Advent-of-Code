using System;

namespace AoC.Library
{
	public static class Functional
	{
		public static T Id<T>(T t) => t;

		public static Func<T, bool> Not<T>(Func<T, bool> f) =>
			t => !f(t);
	}
}
