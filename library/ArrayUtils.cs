using System;

namespace AoC.Library
{
	public static class ArrayUtils
	{
		public static int IndexOf<T>(this T[] array, T value) =>
			Array.IndexOf(array, value);
	}
}
