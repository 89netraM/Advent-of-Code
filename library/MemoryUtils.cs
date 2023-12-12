using System;

namespace AoC.Library;

public static class MemoryExtensions
{
	public static bool All<T>(this ReadOnlyMemory<T> memory, Predicate<T> predicate)
	{
		foreach (var t in memory.Span)
		{
			if (!predicate(t))
			{
				return false;
			}
		}
		return true;
	}

	public static int CountPrefix<T>(this ReadOnlyMemory<T> memory, Predicate<T> predicate)
	{
		int count = 0;
		foreach (var t in memory.Span)
		{
			if (!predicate(t))
			{
				return count;
			}
			count++;
		}
		return count;
	}
}
