using System;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;
using System.Text.Json;

namespace AoC.Year2015
{
	[Day(12)]
	public class Day12
	{
		[Part(1)]
		public long Part1(string input) =>
			Regex.Matches(input, @"-?\d+")
				.Sum(m => Int64.Parse(m.Value));

		[Part(2)]
		public long Part2(string input) =>
			SumJsonElement(JsonDocument.Parse(input).RootElement);

		private long SumJsonElement(JsonElement element) =>
			element.ValueKind switch
			{
				JsonValueKind.Array => SumJsonArray(element),
				JsonValueKind.Object => SumJsonObject(element),
				JsonValueKind.Number => element.GetInt64(),
				_ => 0L,
			};

		private long SumJsonArray(JsonElement array) =>
			array.EnumerateArray().Sum(SumJsonElement);

		private long SumJsonObject(JsonElement obj)
		{
			if (obj.EnumerateObject().Any(prop => prop.Value.ValueKind == JsonValueKind.String && prop.Value.GetString() == "red"))
			{
				return 0L;
			}
			else
			{
				return obj.EnumerateObject().Sum(prop => SumJsonElement(prop.Value));
			}
		}
	}
}
