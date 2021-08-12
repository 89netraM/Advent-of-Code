using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(16)]
	public class Day16
	{
		[Part(1)]
		public object Part1(string input)
		{
			char[] programs = Enumerable.Range(0, 16).Select(static i => (char)('a' + i)).ToArray();
			char[] temp = new char[programs.Length];
			string[] moves = input.Split(',');

			foreach (var move in moves)
			{
				if (move.StartsWith("s"))
				{
					int size = int.Parse(move.Substring(1));
					Array.Copy(programs, programs.Length - size, temp, 0, size);
					Array.Copy(programs, 0, temp, size, programs.Length - size);
					Array.Copy(temp, programs, programs.Length);
				}
				else if (move.StartsWith("x"))
				{
					var parts = move.Substring(1).Split('/');
					int a = int.Parse(parts[0]);
					int b = int.Parse(parts[1]);
					(programs[a], programs[b]) = (programs[b], programs[a]);
				}
				else if (move.StartsWith("p"))
				{
					var parts = move.Substring(1).Split('/');
					int a = IndexOf(programs, parts[0][0]);
					int b = IndexOf(programs, parts[1][0]);
					(programs[a], programs[b]) = (programs[b], programs[a]);
				}
			}

			return String.Concat(programs);
		}

		private static int IndexOf(char[] array, char value)
		{
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i] == value) return i;
			}
			return -1;
		}

		[Part(2)]
		public object Part2(string input)
		{
			char[] programs = Enumerable.Range(0, 16).Select(static i => (char)('a' + i)).ToArray();
			char[] temp = new char[programs.Length];
			string[] moves = input.Split(',');

			IDictionary<string, long> seen = new Dictionary<string, long>();
			const long steps = 1000000000;
			for (long i = 0; i < steps; i++)
			{
				foreach (var move in moves.Select(Move))
				{
					move(programs, temp);
				}

				string state = String.Concat(programs);
				if (seen.TryGetValue(state, out long stepsToPrevious))
				{
					long oldI = i;
					i = steps - ((steps - stepsToPrevious) % (i - stepsToPrevious));
					Console.WriteLine($"Found a period from step {stepsToPrevious} to step {oldI}, and used it to skip to step {i}.");
				}
				else
				{
					seen.Add(state, i);
				}
			}

			return String.Concat(programs);
		}

		private static Action<char[], char[]> Move(string move)
		{
			if (move.StartsWith("s"))
			{
				int size = int.Parse(move.Substring(1));
				return (programs, temp) =>
				{
					Array.Copy(programs, programs.Length - size, temp, 0, size);
					Array.Copy(programs, 0, temp, size, programs.Length - size);
					Array.Copy(temp, programs, programs.Length);
				};
			}
			else if (move.StartsWith("x"))
			{
				var parts = move.Substring(1).Split('/');
				int a = int.Parse(parts[0]);
				int b = int.Parse(parts[1]);
				return (programs, temp) =>
				{
					(programs[a], programs[b]) = (programs[b], programs[a]);
				};
			}
			else if (move.StartsWith("p"))
			{
				var parts = move.Substring(1).Split('/');
				return (programs, temp) =>
				{
					int a = IndexOf(programs, parts[0][0]);
					int b = IndexOf(programs, parts[1][0]);
					(programs[a], programs[b]) = (programs[b], programs[a]);
				};
			}
			throw new NotImplementedException();
		}
	}
}
