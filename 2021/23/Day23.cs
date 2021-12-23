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

			Console.CursorVisible = false;
			Console.OutputEncoding = System.Text.Encoding.UTF8;

			long[] steps = new long[] { 0L, 0L, 0L, 0L };
			long cost() =>
				steps[0] * CostPerMove(Occupant.Amber) +
				steps[1] * CostPerMove(Occupant.Bronze) +
				steps[2] * CostPerMove(Occupant.Copper) +
				steps[3] * CostPerMove(Occupant.Desert);

			var chosen = map.First(p => p.Value == Occupant.Amber).Key;
			var targets = FindTargets(map, chosen).OrderBy(static p => p.X).ToArray();
			var chosenTarget = 0;

			var printDict = new Dictionary<Vector2, (ConsoleColor, char?)>();

			while (true)
			{
				if (Done(map, Occupant.Amber) && Done(map, Occupant.Bronze) &&
					Done(map, Occupant.Copper) && Done(map, Occupant.Desert))
				{
					Print(map);
					Console.WriteLine(String.Join(", ", steps));
					break;
				}

				printDict.Clear();
				for (int i = 0; i < targets.Length; i++)
				{
					printDict[targets[i]] = (i == chosenTarget ? ConsoleColor.Green : ConsoleColor.Blue, 'x');
				}
				printDict[chosen] = (ConsoleColor.Yellow, null);
				int height = Print(map, printDict);
				Console.WriteLine(String.Join(", ", steps));
				height++;
				Console.WriteLine(cost());
				height++;

				while (true)
				{
					var key = Console.ReadKey(true);
					var kind = (Occupant)(key.Key - ConsoleKey.A);
					if (key.Key == ConsoleKey.Escape)
					{
						return null;
					}
					else if (Occupant.Amber <= kind && kind <= Occupant.Desert)
					{
						chosen = map.First(p => p.Value == kind && p.Key != chosen).Key;
						targets = FindTargets(map, chosen).OrderBy(static p => p.X).ToArray();
						chosenTarget = 0;
						break;
					}
					else if (key.Key is ConsoleKey.LeftArrow or ConsoleKey.RightArrow)
					{
						if (targets.Length > 0)
						{
							chosenTarget = MathM.Mod(chosenTarget + ((int)key.Key - 38), targets.Length);
							break;
						}
					}
					else if (key.Key is ConsoleKey.Spacebar or ConsoleKey.Enter)
					{
						steps[(int)map[chosen]] += chosen.ManhattanDistance(targets[chosenTarget]);
						(map[chosen], map[targets[chosenTarget]]) = (Occupant.Empty, map[chosen]);
						chosen = targets[chosenTarget];
						targets = FindTargets(map, chosen).OrderBy(static p => p.X).ToArray();
						chosenTarget = 0;
						break;
					}
				}

				Console.CursorTop -= height;
			}

			Console.CursorVisible = true;
			return cost();
		}

		[Part(1)]
		public object Part1(string input) =>
			Solve(input.Lines());

		[Part(2)]
		public object Part2(string input) =>
			Solve(input.Lines().Take(3).Concat(new[] { "  #D#C#B#A#", "  #D#B#A#C#" }).Concat(input.Lines().Skip(3)), 4);

		const int DefaultHeight = 2;

		long? Solve(IEnumerable<string> input, int height = DefaultHeight)
		{
			var map = input.SelectMany(static (l, y) => l.Select((c, x) => (p: new Vector2(x, y), c)))
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

			bool success = BFS.Search(
				new Map(map),
				map => map.Where(static kvp => kvp.Value != Occupant.Empty)
					.SelectMany(kvp =>
						FindTargets(map, kvp.Key, height)
						.Select(t => (
							kvp.Key,
							map.Move(kvp.Key, t),
							kvp.Key.ManhattanDistance(t) * CostPerMove(kvp.Value)
						))
					),
				map => Done(map, Occupant.Amber, height) && Done(map, Occupant.Bronze, height) &&
					Done(map, Occupant.Copper, height) && Done(map, Occupant.Desert, height),
				out var path
			);
			if (success)
			{
				Print(map);
				Console.WriteLine($"Cost: 0\n");
				foreach (var (from, step, cost) in path)
				{
					Print(step, new Dictionary<Vector2, (ConsoleColor, char?)>
					{
						[from] = (ConsoleColor.DarkGray, 'x'),
					});
					Console.WriteLine($"Cost: {cost}\n");
				}
				return path.Last().Item3;
			}
			else
			{
				return null;
			}
		}

		static int Print(IReadOnlyDictionary<Vector2, Occupant> map, IReadOnlyDictionary<Vector2, (ConsoleColor, char?)> special = null)
		{
			var min = map.Keys.Aggregate((a, b) => a.MinParts(b));
			var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
			int lines = 0;
			for (long y = min.Y - 1; y <= max.Y + 1; y++)
			{
				for (long x = min.X - 1; x <= max.X + 1; x++)
				{
					if (y > 2)
					{
						if (x <= min.X)
						{
							Console.Write(' ');
							continue;
						}
						else if (max.X <= x)
						{
							break;
						}
					}

					var p = new Vector2(x, y);
					bool colored = false;
					char? c = null;
					if (special?.TryGetValue(p, out var s) ?? false)
					{
						Console.ForegroundColor = s.Item1;
						colored = true;
						c = s.Item2;
					}
					if (c is char cc)
					{
						Console.Write(cc);
					}
					else if (map.TryGetValue(p, out var o))
					{
						Console.Write(o switch
						{
							Occupant.Amber => 'A',
							Occupant.Bronze => 'B',
							Occupant.Copper => 'C',
							Occupant.Desert => 'D',
							Occupant.Empty => '.',
							_ => throw new Exception("Invalid occupant"),
						});
					}
					else
					{
						Console.Write(y == 1 && min.X <= x && x <= max.X ? '.' : '#');
					}
					if (colored)
					{
						Console.ResetColor();
					}
				}
				Console.WriteLine();
				lines++;
			}
			return lines;
		}

		static IEnumerable<Vector2> FindTargets(IReadOnlyDictionary<Vector2, Occupant> map, Vector2 from, int height = DefaultHeight)
		{
			var targets = new List<Vector2>();
			if (from.Y == 1)
			{
				if (Home(map, map[from], height) is Vector2 h && CanMoveTo(map, from, h))
				{
					yield return h;
				}
			}
			else if (IsOnTop(map, from))
			{
				foreach (var t in restingSpots)
				{
					if (CanMoveTo(map, from, t))
					{
						yield return t;
					}
				}
			}
		}

		static bool Done(IReadOnlyDictionary<Vector2, Occupant> map, Occupant kind, int height = DefaultHeight) =>
			rooms[kind].Take(height).All(p => map[p] == kind);
		static Vector2? Home(IReadOnlyDictionary<Vector2, Occupant> map, Occupant kind, int height = DefaultHeight)
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

			public Occupant this[Vector2 key] => map[key];
			public IEnumerable<Vector2> Keys => map.Keys;
			public IEnumerable<Occupant> Values => map.Values;
			public int Count => map.Count;

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

			public Map Move(Vector2 from, Vector2 to) =>
				new Map(map.SetItems(new KeyValuePair<Vector2, Occupant>[]{
					new(from, Occupant.Empty),
					new(to, map[from])
				}));

			public bool ContainsKey(Vector2 key) =>
				map.ContainsKey(key);
			public IEnumerator<KeyValuePair<Vector2, Occupant>> GetEnumerator() =>
				map.GetEnumerator();
			public bool TryGetValue(Vector2 key, [MaybeNullWhen(false)] out Occupant value) =>
				map.TryGetValue(key, out value);
			System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() =>
				GetEnumerator();

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
			public override int GetHashCode() =>
				hash;
		}
	}
}
