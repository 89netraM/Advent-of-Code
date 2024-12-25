using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using BenchmarkDotNet.Columns;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Diagnosers;
using BenchmarkDotNet.Jobs;
using BenchmarkDotNet.Loggers;
using BenchmarkDotNet.Parameters;
using BenchmarkDotNet.Reports;
using BenchmarkDotNet.Running;
using TextCopy;

namespace AoC.Library
{
	public static class Runner
	{
		private static readonly ImmutableConfig BenchmarkConfig = ManualConfig.CreateEmpty()
			.AddColumnProvider(DefaultColumnProviders.Instance)
			.AddLogger(ConsoleLogger.Unicode)
			.WithOptions(ConfigOptions.DisableLogFile)
			.AddDiagnoser(MemoryDiagnoser.Default)
			.CreateImmutableConfig();

		public static void Run(string[] args)
		{
			bool benchmark = false;
			if (args.Any(IsBenchmarkFlag))
			{
				benchmark = true;
				args = args.Where(a => !IsBenchmarkFlag(a)).ToArray();
			}

			if (!(DivineDayAndPart(args) is (int day, int part)))
			{
				return;
			}

			if (!(GetInputForDay(day) is string input))
			{
				return;
			}

			if (!(GetDayClass(Assembly.GetCallingAssembly(), day) is Type dayClass))
			{
				return;
			}

			if (benchmark)
			{
				BenchmarkDay(day, dayClass, input);
			}
			else
			{
				RunPart(day, dayClass, part, input);
			}
		}

		private static void RunPart(int day, Type dayClass, int part, string input)
		{
			if (!(GetPartMethod(dayClass, part) is MethodInfo partMethod))
			{
				return;
			}

			if (!(InstantiateDayClass(dayClass) is object instance))
			{
				return;
			}

			try
			{
				object? output = partMethod.Invoke(instance, new object[] { input });
				if (output?.ToString()?.Trim() is string outputString && outputString.Length > 0)
				{
					Console.WriteLine(outputString);
					ClipboardService.SetText(outputString);
				}
				else
				{
					Console.WriteLine($"No output from solution for day {day} part {part}.");
				}
			}
			catch (TargetInvocationException e) when (e.InnerException is Exception inner)
			{
				Console.WriteLine($"Exception while running solution for day {day} part {part}.");
				Console.WriteLine(inner);
			}
		}

		private static void BenchmarkDay(int day, Type dayClass, string input)
		{
			var cases = GetPartMethods(dayClass)
				.Select(mi => MakeBenchmarkCase(dayClass, mi, input))
				.ToArray();
			var runInfo = new BenchmarkRunInfo(cases, dayClass, BenchmarkConfig);
			var summary = BenchmarkRunner.Run(runInfo);
			foreach (var report in summary.Reports)
			{
				Console.WriteLine($"LEADERBOARD::{report.BenchmarkCase.Descriptor.WorkloadMethodDisplayInfo.ToUpperInvariant()}::TIME {double.Round(report.ResultStatistics.Mean)} ns");
			}
		}

		private static bool IsBenchmarkFlag(string argument) =>
			argument == "--benchmark" || argument == "-b";

		private static (int, int)? DivineDayAndPart(string[] args)
		{
			int day = 0;
			int part = 1;
			if (args.Length == 2)
			{
				if (!(Int32.TryParse(args[0], out day) && Int32.TryParse(args[1], out part)))
				{
					Console.WriteLine("When given two arguments, they must be integers representing the day and part respectively.");
					return null;
				}
			}
			else
			{
				if (args.Length == 1)
				{
					if (!(Int32.TryParse(args[0], out part)))
					{
						Console.WriteLine("When given one argument, it must be an integer representing the part.");
						return null;
					}
				}
				else
				{
					Console.WriteLine("Assuming you want to solve part 1.");
				}

				if (Int32.TryParse(Path.GetFileName(Environment.CurrentDirectory), out day))
				{
					// Got day number for directory name
				}
				else if (DateTime.Today.Month == 12)
				{
					day = DateTime.Today.Day;
				}
				else
				{
					Console.WriteLine("Could not figure out what day you want to solve.");
					Console.WriteLine("Either navigate to the directory of the day you want to solve, be in December and try to solve todays day, or provide two arguments: the day and the part you want to solve.");
					return null;
				}
			}

			return (day, part);
		}

		private static string? GetInputForDay(int day)
		{

			string? csprojRoot = FigureOutCSProjRoot();
			if (csprojRoot is null)
			{
				Console.WriteLine("Could not find the CSProj root.");
				return null;
			}

			string inputFile = Path.Combine(csprojRoot, day.ToString(), "input.txt");
			if (!File.Exists(inputFile))
			{
				Console.WriteLine($"Could not find input file. \"{inputFile}\"");
				return null;
			}

			return File.ReadAllText(inputFile).TrimEnd();
		}

		private static string? FigureOutCSProjRoot()
		{
			string? root = Environment.CurrentDirectory;
			while (root is not null)
			{
				if (Directory.GetFiles(root, "*.csproj").Length > 0)
				{
					return root;
				}
				else
				{
					root = Path.GetDirectoryName(root);
				}
			}
			return null;
		}

		private static Type? GetDayClass(Assembly? assembly, int day)
		{
			if (assembly is Assembly calling)
			{
				Type? dayClass = calling.GetTypes()
					.Where(static t => Attribute.IsDefined(t, typeof(DayAttribute)))
					.Where(t => ((DayAttribute)Attribute.GetCustomAttribute(t, typeof(DayAttribute))!).Day == day)
					.FirstOrDefault();
				if (dayClass is null)
				{
					Console.WriteLine($"Could not find a class for day {day}.");
					return null;
				}

				return dayClass;
			}
			else
			{
				return null;
			}
		}

		private static MethodInfo? GetPartMethod(Type dayClass, int part)
		{
			MethodInfo? partMethod = GetPartMethods(dayClass)
				.Where(m => ((PartAttribute)Attribute.GetCustomAttribute(m, typeof(PartAttribute))!).Part == part)
				.FirstOrDefault();
			if (partMethod is null)
			{
				Console.WriteLine($"Could not find a method for part {part} on {dayClass.Name}.");
				return null;
			}

			return partMethod;
		}

		private static IEnumerable<MethodInfo> GetPartMethods(Type dayClass) =>
			dayClass.GetMethods(BindingFlags.Instance | BindingFlags.Public)
				.Where(static m => Attribute.IsDefined(m, typeof(PartAttribute)));

		private static object? InstantiateDayClass(Type dayClass)
		{
			object? instance = dayClass.GetConstructor(Type.EmptyTypes)?.Invoke(null);
			if (instance is null)
			{
				Console.WriteLine($"Could not create an instance of {dayClass.Name}. Needs a parameterless constructor.");
				return null;
			}

			return instance;
		}

		private static BenchmarkCase MakeBenchmarkCase(Type dayClass, MethodInfo partMethod, string input) =>
			BenchmarkCase.Create(
				new Descriptor(dayClass, partMethod),
				Job.Default,
				new ParameterInstances(new[]
				{
					new ParameterInstance(
						new ParameterDefinition("input", false, new[] { input }, true, typeof(string), 0),
						input,
						SummaryStyle.Default)
				}),
				BenchmarkConfig);
	}
}
