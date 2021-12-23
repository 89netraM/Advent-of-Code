using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using AoC.Library;

namespace AoC.Year2021
{
	[Day(23)]
	public class Day23
	{
		enum Occupant : int
		{
			Amber,
			Bronze,
			Copper,
			Desert,
			Empty
		}

		static IReadOnlyDictionary<Occupant, Vector2[]> rooms = new Dictionary<Occupant, Vector2[]>()
		{
			[Occupant.Amber] = new[] { new Vector2(3, 2), new Vector2(3, 3), new Vector2(3, 4), new Vector2(3, 5) },
			[Occupant.Bronze] = new[] { new Vector2(5, 2), new Vector2(5, 3), new Vector2(5, 4), new Vector2(5, 5) },
			[Occupant.Copper] = new[] { new Vector2(7, 2), new Vector2(7, 3), new Vector2(7, 4), new Vector2(7, 5) },
			[Occupant.Desert] = new[] { new Vector2(9, 2), new Vector2(9, 3), new Vector2(9, 4), new Vector2(9, 5) }
		};

		static IReadOnlyCollection<Vector2> restingSpots = new[]
		{
			new Vector2(1, 1),
			new Vector2(2, 1),
			new Vector2(4, 1),
			new Vector2(6, 1),
			new Vector2(8, 1),
			new Vector2(10, 1),
			new Vector2(11, 1),
		};

		[Part(0)]
		public object Play(string input)
		{
			var map = input.Lines()
				.SelectMany((l, y) => l.Select((c, x) => (p: new Vector2(x, y), c)))
				.Where(p => p.c != '#' && p.c != ' ')
				.ToDictionary(p => p.p, p => p.c switch
				{
					'A' => Occupant.Amber,
					'B' => Occupant.Bronze,
					'C' => Occupant.Copper,
					'D' => Occupant.Desert,
					'.' => Occupant.Empty,
					_ => throw new Exception("Invalid occupant"),
				});
			var reverseMap = new Dictionary<Occupant, Vector2[]>();
			foreach (var kvp in map)
			{
				if (kvp.Value != Occupant.Empty)
				{
					if (reverseMap.TryGetValue(kvp.Value, out var pair))
					{
						pair[1] = kvp.Key;
					}
					else
					{
						reverseMap[kvp.Value] = new[] { kvp.Key, Vector2.Zero };
					}
				}
			}

			Console.CursorVisible = false;
			Console.OutputEncoding = System.Text.Encoding.UTF8;

			long a = 0L;
			long b = 0L;
			long c = 0L;
			long d = 0L;
			long cost = 0L;

			var chosen = (o: Occupant.Amber, i: 0);

			while (true)
			{
				var cPos = reverseMap[chosen.o][chosen.i];

				int height = Print(map, cPos);
				Console.WriteLine($"{a}, {b}, {c}, {d}");
				Console.WriteLine(cost);

				if (Done(map, Occupant.Amber) && Done(map, Occupant.Bronze) &&
					Done(map, Occupant.Copper) && Done(map, Occupant.Desert))
				{
					Console.CursorTop--;
					break;
				}

				while (true)
				{
					var key = Console.ReadKey(true);
					var target = cPos;
					switch (key.Key)
					{
						case ConsoleKey.UpArrow:
							target = cPos + Vector2.Up;
							break;
						case ConsoleKey.DownArrow:
							target = cPos + Vector2.Down;
							break;
						case ConsoleKey.LeftArrow:
							target = cPos + Vector2.Left;
							break;
						case ConsoleKey.RightArrow:
							target = cPos + Vector2.Right;
							break;
						case ConsoleKey.Escape:
							Console.CursorVisible = true;
							return null;
					}
					if (target != cPos)
					{
						if (map.TryGetValue(target, out var o) && o == Occupant.Empty)
						{
							map[target] = chosen.o;
							map[cPos] = Occupant.Empty;
							reverseMap[chosen.o][chosen.i] = target;
							switch (chosen.o)
							{
								case Occupant.Amber:
									a++;
									break;
								case Occupant.Bronze:
									b++;
									break;
								case Occupant.Copper:
									c++;
									break;
								case Occupant.Desert:
									d++;
									break;
							}
							cost += CostPerMove(chosen.o);
							break;
						}
					}
					else
					{
						int k = key.Key - ConsoleKey.A;
						if ((int)Occupant.Amber <= k && k <= (int)Occupant.Desert)
						{
							if (k == (int)chosen.o)
							{
								chosen.i = (chosen.i + 1) % 2;
							}
							else
							{
								chosen = ((Occupant)k, 0);
							}
							break;
						}
					}
				}
				Console.CursorTop -= height + 2;
			}

			Console.CursorVisible = true;
			return cost;
		}

		[Part(1)]
		public object Part1(string input)
		{
			var map = input.Lines()
				.SelectMany(static (l, y) => l.Select((c, x) => (p: new Vector2(x, y), c)))
				.Where(static p => p.c != '#' && p.c != ' ')
				.Where(static p => p.p.Y >= 2 || restingSpots.Contains(p.p))
				.ToImmutableDictionary(static p => p.p, static p => p.c switch
				{
					'A' => Occupant.Amber,
					'B' => Occupant.Bronze,
					'C' => Occupant.Copper,
					'D' => Occupant.Desert,
					'.' => Occupant.Empty,
					_ => throw new Exception("Invalid occupant"),
				});

			BFS.Search(
				new Map(map),
				static map =>
				{
					var result = new List<(Map, long)>();
					foreach (var kvp in map.Where(static kvp => kvp.Value != Occupant.Empty))
					{
						if (kvp.Key.Y == 1)
						{
							if (Home(map, kvp.Value) is Vector2 h && CanMoveTo(map, kvp.Key, h))
							{
								result.Add((
									map.Move(kvp.Key, h),
									kvp.Key.ManhattanDistance(h) * CostPerMove(kvp.Value)
								));
							}
						}
						else if (IsOnTop(map, kvp.Key))
						{
							foreach (var t in restingSpots)
							{
								if (CanMoveTo(map, kvp.Key, t))
								{
									result.Add((
										map.Move(kvp.Key, t),
										kvp.Key.ManhattanDistance(t) * CostPerMove(kvp.Value)
									));
								}
							}
						}
					}
					return result;
				},
				static map => Done(map, Occupant.Amber) && Done(map, Occupant.Bronze) &&
					Done(map, Occupant.Copper) && Done(map, Occupant.Desert),
				out var path
			);
			foreach (var (step, cost) in path)
			{
				Print(step, Vector2.Zero);
				Console.WriteLine($"Cost: {cost}\n");
			}
			return path.Last().Item2;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var map = input.Lines().Take(3).Concat(new[] { "  #D#C#B#A#", "  #D#B#A#C#" }).Concat(input.Lines().Skip(3))
				.SelectMany(static (l, y) => l.Select((c, x) => (p: new Vector2(x, y), c)))
				.Where(static p => p.c != '#' && p.c != ' ')
				.Where(static p => p.p.Y >= 2 || restingSpots.Contains(p.p))
				.ToImmutableDictionary(static p => p.p, static p => p.c switch
				{
					'A' => Occupant.Amber,
					'B' => Occupant.Bronze,
					'C' => Occupant.Copper,
					'D' => Occupant.Desert,
					'.' => Occupant.Empty,
					_ => throw new Exception("Invalid occupant"),
				});

			BFS.Search(
				new Map(map),
				static map =>
				{
					var result = new List<(Map, long)>();
					foreach (var kvp in map.Where(static kvp => kvp.Value != Occupant.Empty))
					{
						if (kvp.Key.Y == 1)
						{
							if (Home(map, kvp.Value, 4) is Vector2 h && CanMoveTo(map, kvp.Key, h))
							{
								result.Add((
									map.Move(kvp.Key, h),
									kvp.Key.ManhattanDistance(h) * CostPerMove(kvp.Value)
								));
							}
						}
						else if (IsOnTop(map, kvp.Key))
						{
							foreach (var t in restingSpots)
							{
								if (CanMoveTo(map, kvp.Key, t))
								{
									result.Add((
										map.Move(kvp.Key, t),
										kvp.Key.ManhattanDistance(t) * CostPerMove(kvp.Value)
									));
								}
							}
						}
					}
					return result;
				},
				static map => Done(map, Occupant.Amber, 4) && Done(map, Occupant.Bronze, 4) &&
					Done(map, Occupant.Copper, 4) && Done(map, Occupant.Desert, 4),
				out var path
			);
			foreach (var (step, cost) in path)
			{
				Print(step, Vector2.Zero);
				Console.WriteLine($"Cost: {cost}\n");
			}
			return path.Last().Item2;
		}

		static int Print(IReadOnlyDictionary<Vector2, Occupant> map, Vector2 chosen)
		{
			var min = map.Keys.Aggregate((a, b) => a.MinParts(b));
			var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
			int lines = 0;
			for (long y = min.Y - 1; y <= max.Y + 1; y++)
			{
				for (long x = min.X - 1; x <= max.X + 1; x++)
				{
					var p = new Vector2(x, y);
					if (p == chosen)
					{
						Console.ForegroundColor = ConsoleColor.Yellow;
					}
					Console.Write(map.TryGetValue(p, out var occupant) ? occupant switch
					{
						Occupant.Amber => 'A',
						Occupant.Bronze => 'B',
						Occupant.Copper => 'C',
						Occupant.Desert => 'D',
						Occupant.Empty => '.',
						_ => throw new Exception("Invalid occupant"),
					} : '#');
					if (p == chosen)
					{
						Console.ResetColor();
					}
				}
				Console.WriteLine();
				lines++;
			}
			return lines;
		}

		static bool Done(IReadOnlyDictionary<Vector2, Occupant> map, Occupant kind, int height = 2) =>
			rooms[kind].Take(height).All(p => map[p] == kind);
		static Vector2? Home(IReadOnlyDictionary<Vector2, Occupant> map, Occupant kind, int height = 2)
		{
			var rs = rooms[kind];
			for (int i = 0; i < height; i++)
			{
				var p = rs[height - 1 - i];
				var k = map[p];
				if (k == Occupant.Empty)
				{
					return p;
				}
				else if (k != kind)
				{
					return null;
				}
			}
			return null;
		}

		static IEnumerable<Vector2> PathBetween(Vector2 from, Vector2 to) =>
			restingSpots.Where(p => (from.X < p.X && p.X <= to.X) || (to.X <= p.X && p.X < from.X));
		static bool CanMoveTo(IReadOnlyDictionary<Vector2, Occupant> map, Vector2 from, Vector2 to) =>
			PathBetween(from, to).All(p => map[p] == Occupant.Empty);

		static bool IsOnTop(IReadOnlyDictionary<Vector2, Occupant> map, Vector2 pos) =>
			pos.Y == 2 || map[pos + Vector2.Up] == Occupant.Empty;

		static long CostPerMove(Occupant o) =>
			o switch
			{
				Occupant.Amber => 1L,
				Occupant.Bronze => 10L,
				Occupant.Copper => 100L,
				Occupant.Desert => 1000L,
				_ => throw new Exception("Invalid occupant"),
			};

		readonly struct Map : IEquatable<Map>, IReadOnlyDictionary<Vector2, Occupant>
		{
			private readonly ImmutableDictionary<Vector2, Occupant> map;
			private readonly int hash;

			public Map(ImmutableDictionary<Vector2, Occupant> map)
			{
				this.map = map;

				var hash = new HashCode();
				var min = map.Keys.Aggregate(static (a, b) => a.MinParts(b));
				var max = map.Keys.Aggregate(static (a, b) => a.MaxParts(b));
				for (long y = min.Y; y <= max.Y; y++)
				{
					for (long x = min.X; x <= max.X; x++)
					{
						hash.Add(map.GetValueOrDefault(new Vector2(x, y), Occupant.Empty));
					}
				}
				this.hash = hash.ToHashCode();
			}

			public Occupant this[Vector2 key] => map[key];

			public IEnumerable<Vector2> Keys => map.Keys;

			public IEnumerable<Occupant> Values => map.Values;

			public int Count => map.Count;

			public bool ContainsKey(Vector2 key) =>
				map.ContainsKey(key);

			public Map Move(Vector2 from, Vector2 to) =>
				new Map(map.SetItems(new KeyValuePair<Vector2, Occupant>[]{
					new(from, Occupant.Empty),
					new(to, map[from])
				}));

			public override bool Equals(object obj) =>
				obj is Map other && Equals(other);
			public bool Equals(Map other)
			{
				foreach (var kvp in map)
				{
					if (other.map[kvp.Key] != kvp.Value)
					{
						return false;
					}
				}
				return true;
			}

			public IEnumerator<KeyValuePair<Vector2, Occupant>> GetEnumerator() =>
				map.GetEnumerator();

			public override int GetHashCode() =>
				hash;

			public bool TryGetValue(Vector2 key, [MaybeNullWhen(false)] out Occupant value) =>
				map.TryGetValue(key, out value);

			System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() =>
				GetEnumerator();
		}
	}
}
