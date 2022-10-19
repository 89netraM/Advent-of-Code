using System;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text.Json;
using System.Text.RegularExpressions;
using TupleAsJsonArray;

namespace AoC.Library
{
	public static class ObjectUtils
	{
		private static readonly Regex MethodChainingSpacing = new Regex(@"\n\s+\.");
		private static readonly JsonSerializerOptions JsonSerializerOptions = new()
		{
			IncludeFields = true,
			Converters =
			{
				new TupleConverterFactory(),
			},
		};

		public static T Dump<T>(this T t,
			[CallerArgumentExpression("t")] string tExpression = "",
			[CallerFilePath] string sourceFilePath = "",
			[CallerLineNumber] int sourceFileLine = 0)
		{
			tExpression = MethodChainingSpacing.Replace(tExpression, ".");

			if (CsProjRoot(sourceFilePath) is string vsCodeRoot)
			{
				sourceFilePath = "." + Path.DirectorySeparatorChar + Path.GetRelativePath(vsCodeRoot, sourceFilePath);
			}

			string summary = $"[{sourceFilePath}:{sourceFileLine}] {tExpression} =";
			string dump = JsonSerializer.Serialize<T>(t, JsonSerializerOptions);

			if (summary.Length + dump.Length > 120)
			{
				Console.Error.WriteLine(summary);
				Console.Error.WriteLine($"\t{dump}");
			}
			else
			{
				Console.Error.WriteLine($"{summary} {dump}");
			}

			return t;
		}

		private static string? CsProjRoot(string sourceFilePath)
		{
			for (string? path = Path.GetDirectoryName(sourceFilePath); Path.Exists(path); path = Path.GetDirectoryName(path))
			{
				if (Directory.GetFiles(path, "*.csproj").Any())
				{
					return path;
				}
			}
			return null;
		}
	}
}
