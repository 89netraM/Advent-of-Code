using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

namespace AoC.Year2022;

[Day(17)]
public class Day17
{
	[Part(1)]
	public object Part1(string input)
	{
		var map = new HashSet<Vector2> { new(0, 0), new(1, 0), new(2, 0), new(3, 0), new(4, 0), new(5, 0), new(6, 0) };

		long top = 0;
		int stream = 0;
		for (int r = 0; r < 2022; r++)
		{
			var rock = new Rock(new Vector2(2, top - 4), Rock.RockFormations[r % Rock.RockFormations.Length]);
			while (true)
			{
				if (input[stream++ % input.Length] == '>')
					rock.PushRight(map);
				else
					rock.PushLeft(map);

				if (rock.PushDown(map))
				{
					rock.AddToMap(map);
					top = Math.Min(top, rock.Top);
					break;
				}
			}
			stream %= input.Length;
		}

		return -top;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var map = new HashSet<Vector2> { new(0, 0), new(1, 0), new(2, 0), new(3, 0), new(4, 0), new(5, 0), new(6, 0) };

		long top = 0;
		long stream = 0;
		const long TotalRocks = 1000000000000L;
		var tops = new Dictionary<State, (long rocks, long top)>();
		var topsRocks = new Dictionary<long, long>();
		for (long r = 0; r < TotalRocks; r++)
		{
			var rock = new Rock(new Vector2(2, top - 4), Rock.RockFormations[r % Rock.RockFormations.Length]);
			while (true)
			{
				if (input[(int)(stream++ % input.Length)] == '>')
					rock.PushRight(map);
				else
					rock.PushLeft(map);

				if (rock.PushDown(map))
				{
					top = Math.Min(top, rock.Top);
					rock.AddToMap(map);

					var topLayers = map.Where(r => r.Y < top + 10);
					var bottom = topLayers.Max(r => r.Y);
					var saveMap = topLayers.Select(r => new Vector2(r.X, r.Y - bottom)).ToImmutableHashSet();
					var state = new State(r % Rock.RockFormations.Length, stream, top, saveMap);

					if (tops.TryAdd(state, (r, top)))
					{
						topsRocks.Add(r, top);
					}
					else
					{
						var prev = tops[state];
						var diffRocks = r - prev.rocks;
						var diffHeight = top - prev.top;
						var fromBase = TotalRocks - prev.rocks;
						var times = fromBase / diffRocks;
						var rem = fromBase % diffRocks;
						return -(prev.top + (times * diffHeight) + topsRocks[prev.rocks + rem] - prev.top) - 1;
					}
					break;
				}
			}
			stream %= input.Length;
		}

		return -top;
	}

	private class Rock
	{
		public static readonly IReadOnlySet<Vector2>[] RockFormations = new[]
		{
			new HashSet<Vector2> { new(0, 0), new(1, 0), new(2, 0), new(3, 0) },
			new HashSet<Vector2> { new(1, -2), new(0, -1), new(1, -1), new(2, -1), new(1, 0) },
			new HashSet<Vector2> { new(2, -2), new(2, -1), new(0, 0), new(1, 0), new(2, 0) },
			new HashSet<Vector2> { new(0, -3), new(0, -2), new(0, -1), new(0, 0) },
			new HashSet<Vector2> { new(0, -1), new(1, -1), new(0, 0), new(1, 0) },
		};

		public Vector2 Pos { get; private set; }
		public IReadOnlySet<Vector2> Rocks { get; }

		public long Top => Pos.Y + Rocks.Min(r => r.Y);

		public Rock(Vector2 pos, IReadOnlySet<Vector2> rocks) =>
			(Pos, Rocks) = (pos, rocks);

		public void PushLeft(ISet<Vector2> map)
		{
			if (Pos.X > 0)
			{
				var pos = Pos + Vector2.Left;
				if (!Intersects(map, pos))
				{
					Pos = pos;
				}
			}
		}

		public void PushRight(ISet<Vector2> map)
		{
			if (Pos.X + Rocks.Max(r => r.X) < 6)
			{
				var pos = Pos + Vector2.Right;
				if (!Intersects(map, pos))
				{
					Pos = pos;
				}
			}
		}

		public bool PushDown(ISet<Vector2> map)
		{
			var pos = Pos + Vector2.Down;
			if (!Intersects(map, pos))
			{
				Pos = pos;
				return false;
			}
			else
			{
				return true;
			}
		}

		public bool Intersects(ISet<Vector2> map) =>
			Intersects(map, Pos);
		public bool Intersects(ISet<Vector2> map, Vector2 pos) =>
			map.Overlaps(Rocks.Select(r => r + pos));

		public void AddToMap(ISet<Vector2> map)
		{
			foreach (var rock in Rocks)
			{
				map.Add(Pos + rock);
			}
		}
	}

	private readonly struct State : IEquatable<State>
	{
		public readonly long Rock;
		public readonly long Stream;
		public readonly long Top;
		public readonly ImmutableHashSet<Vector2> Map;
		public State(long Rock, long Stream, long Top, ImmutableHashSet<Vector2> Map)
		{
			this.Rock = Rock;
			this.Stream = Stream;
			this.Top = Top;
			this.Map = Map;
		}

		public override bool Equals([NotNullWhen(true)] object obj) =>
			obj is State state && Equals(state);

		public bool Equals(State o) =>
			Rock == o.Rock && Stream == o.Stream && Map.SetEquals(o.Map);

		public override int GetHashCode() =>
			HashCode.Combine(Rock, Stream);
	}
}
