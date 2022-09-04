using System;
using System.Collections.Generic;
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
	}
}
