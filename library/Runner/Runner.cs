using System;
using System.IO;
using System.Linq;
using System.Reflection;
using TextCopy;

namespace AoC.Library
{
	public static class Runner
	{
		public static void Run(string[] args)
		{
			int day = 0;
			int part = 1;
			if (args.Length == 2)
			{
				if (!(Int32.TryParse(args[0], out day) && Int32.TryParse(args[1], out part)))
				{
					Console.WriteLine("When given two arguments, they must be integers representing the day and part respectively.");
					return;
				}
			}
			else
			{
				if (args.Length == 1)
				{
					if (!(Int32.TryParse(args[1], out part)))
					{
						Console.WriteLine("When given one argument, it must be an integer representing the part.");
						return;
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
					return;
				}
			}

			string? csprojRoot = FigureOutCSProjRoot();
			if (csprojRoot is null)
			{
				Console.WriteLine("Could not find the CSProj root.");
				return;
			}

			string inputFile = Path.Combine(csprojRoot, day.ToString(), "input.txt");
			if (!File.Exists(inputFile))
			{
				Console.WriteLine($"Could not find input file. \"{inputFile}\"");
				return;
			}

			string input = File.ReadAllText(inputFile);

			if (Assembly.GetCallingAssembly() is Assembly calling)
			{
				Type? dayClass = calling.GetTypes()
					.Where(static t => Attribute.IsDefined(t, typeof(DayAttribute)))
					.Where(t => ((DayAttribute)Attribute.GetCustomAttribute(t, typeof(DayAttribute))!).Day == day)
					.FirstOrDefault();
				if (dayClass is null)
				{
					Console.WriteLine($"Could not find a class for day {day}.");
					return;
				}

				MethodInfo? partMethod = dayClass.GetMethods(BindingFlags.Instance | BindingFlags.Public)
					.Where(static m => Attribute.IsDefined(m, typeof(PartAttribute)))
					.Where(m => ((PartAttribute)Attribute.GetCustomAttribute(m, typeof(PartAttribute))!).Part == part)
					.FirstOrDefault();
				if (partMethod is null)
				{
					Console.WriteLine($"Could not find a method for part {part} on {dayClass.Name}.");
					return;
				}

				object? clazz = dayClass.GetConstructor(Type.EmptyTypes)?.Invoke(null);
				if (clazz is null)
				{
					Console.WriteLine($"Could not create an instance of {dayClass.Name}. Needs a parameterless constructor.");
					return;
				}

				object? output = partMethod.Invoke(clazz, new object[] { input });
				if (output?.ToString()?.Trim() is string outputString)
				{
					Console.WriteLine(outputString);
					ClipboardService.SetText(outputString);
				}
				else
				{
					Console.WriteLine($"No output from solution for day {day} part {part}.");
				}
			}
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
	}
}
