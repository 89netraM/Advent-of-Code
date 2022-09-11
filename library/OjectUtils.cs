using System;
using System.Text.Json;

namespace AoC.Library
{
	public static class ObjectUtils
	{
		public static T Dump<T>(this T t)
		{
			Console.Error.WriteLine(JsonSerializer.Serialize<T>(t));
			return t;
		}
	}
}
