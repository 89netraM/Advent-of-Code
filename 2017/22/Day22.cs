using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(22)]
	public class Day22
	{
		[Part(1)]
		public object Part1(string input)
		{
			var infected = input.Lines()
				.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Vector2(x, y))))
				.Where(static p => p.c == '#')
				.Select(static p => p.coord)
				.ToHashSet();

			var max = infected.Aggregate(static (a, b) => a.MaxParts(b));
			var current = new Vector2((max.X + 1) / 2, (max.Y + 1) / 2);
			var direction = Vector2.Up;

			int infecting = 0;

			for (var i = 0; i < 10000; i++)
			{
				if (infected.Add(current))
				{
					direction = TurnLeft(direction);
					infecting++;
				}
				else
				{
					direction = TurnRight(direction);
					infected.Remove(current);
				}
				current = current + direction;
			}

			return infecting;
		}

		private static Vector2 TurnLeft(Vector2 direction) => (direction.X, direction.Y) switch
		{
			(0, -1) => Vector2.Left,
			(-1, 0) => Vector2.Down,
			(0, 1) => Vector2.Right,
			(1, 0) => Vector2.Up,
			_ => throw new Exception("Invalid direction")
		};
		private static Vector2 TurnRight(Vector2 direction) => (direction.X, direction.Y) switch
		{
			(0, -1) => Vector2.Right,
			(-1, 0) => Vector2.Up,
			(0, 1) => Vector2.Left,
			(1, 0) => Vector2.Down,
			_ => throw new Exception("Invalid direction")
		};

		enum State
		{
			Clean,
			Weakened,
			Infected,
			Flagged,
		}

		[Part(2)]
		public object Part2(string input)
		{
			var infected = input.Lines()
				.SelectMany(static (l, y) => l.Select((c, x) => (c, coord: new Vector2(x, y))))
				.ToDictionary(static p => p.coord, static p => p.c == '#' ? State.Infected : State.Clean);

			var max = infected.Keys.Aggregate(static (a, b) => a.MaxParts(b));
			var current = new Vector2((max.X + 1L) / 2L, (max.Y + 1L) / 2L);
			var direction = Vector2.Up;

			long infecting = 0L;

			for (long i = 0L; i < 10000000L; i++)
			{
				switch (infected.GetValueOrDefault(current, State.Clean))
				{
					case State.Clean:
						direction = TurnLeft(direction);
						infected[current] = State.Weakened;
						break;
					case State.Weakened:
						infected[current] = State.Infected;
						infecting++;
						break;
					case State.Infected:
						direction = TurnRight(direction);
						infected[current] = State.Flagged;
						break;
					case State.Flagged:
						direction = Reverse(direction);
						infected[current] = State.Clean;
						break;
				}
				current = current + direction;
			}

			return infecting;
		}

		private static Vector2 Reverse(Vector2 direction) =>
			direction * -1L;
	}
}
