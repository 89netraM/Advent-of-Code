using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using static AoC.Library.Functional;

namespace AoC.Library
{
	public static class IDictionaryUtils
	{
		public static void AddOrUpdate<TKey, TValue>(this IDictionary<TKey, TValue> source, TKey key, TValue newValue, Func<TValue, TValue> updater) =>
			source.AddOrUpdate(key, Const(newValue), updater);

		public static void AddOrUpdate<TKey, TValue>(this IDictionary<TKey, TValue> source, TKey key, Func<TValue> creator, Func<TValue, TValue> updater) =>
			source.AddOrModify(key, creator, oldValue => source[key] = updater(oldValue));

		public static void AddOrModify<TKey, TValue>(this IDictionary<TKey, TValue> source, TKey key, TValue newValue, Action<TValue> modifier) =>
			source.AddOrModify(key, Const(newValue), modifier);

		public static void AddOrModify<TKey, TValue>(this IDictionary<TKey, TValue> source, TKey key, Func<TValue> creator, Action<TValue> modifier)
		{
			if (source.TryGetValue(key, out TValue? oldValue) && oldValue is not null)
			{
				modifier(oldValue);
			}
			else
			{
				source.Add(key, creator());
			}
		}

		public static string ToMapString(this IReadOnlyDictionary<Vector2, char> map, char empty = ' ') =>
			map.ToMapString(Id, empty);

		public static string ToMapString<T>(this IReadOnlyDictionary<Vector2, T> map, Func<T, char> charSelector, char empty = ' ')
		{
			var sb = new StringBuilder();
			var min = map.Keys.Min();
			var max = map.Keys.Max();
			for (long y = min.Y; y <= max.Y; y++)
			{
				for (long x = min.X; x <= max.X; x++)
				{
					sb.Append(map.TryGetValue(new(x, y), out var t) ? charSelector(t) : empty);
				}
				if (y + 1 <= max.Y)
				{
					sb.AppendLine();
				}
			}
			return sb.ToString();
		}
	}
}
