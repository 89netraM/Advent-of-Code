using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AoC.Library
{
	public class CellularAutomaton<TCoord, TState> : IEquatable<CellularAutomaton<TCoord, TState>>, IEnumerable<KeyValuePair<TCoord, TState>>, ICloneable
		where TCoord : struct, IVector<TCoord>
		where TState : struct, Enum
	{
		public delegate TState? Rule(IReadOnlyDictionary<TState, long> counts);

		private IDictionary<TCoord, TState> current;
		private IDictionary<TCoord, TState> next;

		private NeighborhoodKind neighborhood = NeighborhoodKind.VonNeumann;
		public NeighborhoodKind Neighborhood
		{
			get => neighborhood;
			set => neighborhood = Enum.IsDefined<NeighborhoodKind>(value) ?
				value :
				throw new ArgumentException($"{value} is not a valid {nameof(NeighborhoodKind)}.");
		}

		public long NeighborhoodRange { get; set; } = 1L;

		public (TCoord min, TCoord max)? ConfinementBounds { get; set; } = null;

		private IDictionary<TState, Rule> rules;

		public TState DefaultState { get; set; } = default;

		public TState this[TCoord coord]
		{
			get => current[coord];
			set => current[coord] = value;
		}

		public Rule this[TState state]
		{
			get => rules[state];
			set => rules[state] = value;
		}

		public CellularAutomaton(IDictionary<TCoord, TState> current)
		{
			this.current = new Dictionary<TCoord, TState>(current);
			next = new Dictionary<TCoord, TState>();
			rules = new Dictionary<TState, Rule>();
		}

		public CellularAutomaton(CellularAutomaton<TCoord, TState> original)
		{
			current = new Dictionary<TCoord, TState>(original.current);
			next = new Dictionary<TCoord, TState>();
			Neighborhood = original.Neighborhood;
			NeighborhoodRange = original.NeighborhoodRange;
			rules = new Dictionary<TState, Rule>(original.rules);
		}

		public void Step()
		{
			long totalNeighborCount = Vector.Zero<TCoord>().Neighbors(Neighborhood, NeighborhoodRange).Count();

			Dictionary<TCoord, Dictionary<TState, long>> counts = new Dictionary<TCoord, Dictionary<TState, long>>();
			Dictionary<TState, long> defaultNeighborCounts = new Dictionary<TState, long>();
			foreach (TState state in Enum.GetValues<TState>())
			{
				defaultNeighborCounts.Add(state, state.Equals(DefaultState) ? totalNeighborCount : 0L);
			}

			if (ConfinementBounds is (TCoord min, TCoord max))
			{
				LinkedList<long> taken = new LinkedList<long>();

				void FillCountsRecursive()
				{
					if (taken.Count < min.Count - 2)
					{
						for (long i = min[min.Count - taken.Count - 1]; i <= max[min.Count - taken.Count - 1]; i++)
						{
							taken.AddFirst(i);
							FillCountsRecursive();
							taken.RemoveFirst();
						}
					}
					else
					{
						long[] coordArray = new long[min.Count];
						taken.CopyTo(coordArray, 2);
						for (long y = min[1]; y <= max[1]; y++)
						{
							coordArray[1] = y;
							for (long x = min[0]; x <= max[0]; x++)
							{
								coordArray[0] = x;
								counts.Add(Vector.FromArray<TCoord>(coordArray), new Dictionary<TState, long>(defaultNeighborCounts));
							}
						}
					}
				}

				if (min.Count > 1)
				{
					FillCountsRecursive();
				}
				else
				{
					for (long[] coordArray = new [] { min[0] }; coordArray[0] <= max[0]; coordArray[0]++)
					{
						counts.Add(Vector.FromArray<TCoord>(coordArray), new Dictionary<TState, long>(defaultNeighborCounts));
					}
				}
			}

			foreach (KeyValuePair<TCoord, TState> kvp in current)
			{
				foreach (TCoord neighbor in kvp.Key.Neighbors(Neighborhood, NeighborhoodRange))
				{
					if (!(ConfinementBounds is (TCoord, TCoord) cb) ||
						(cb.min.Zip(neighbor).All(static p => p.Item1 <= p.Item2) && cb.max.Zip(neighbor).All(static p => p.Item1 >= p.Item2)))
					{
						Dictionary<TState, long>? neighborCounts;
						if (!counts.TryGetValue(neighbor, out neighborCounts))
						{
							neighborCounts = new Dictionary<TState, long>(defaultNeighborCounts);
							counts.Add(neighbor, neighborCounts);
						}

						neighborCounts[DefaultState]--;
						neighborCounts.TryGetValue(kvp.Value, out long neighborCount);
						neighborCounts[kvp.Value] = neighborCount + 1;
					}
				}
			}

			foreach (KeyValuePair<TCoord, Dictionary<TState, long>> kvp in counts)
			{
				TState? nextState = rules[current.TryGetValue(kvp.Key, out TState currentState) ? currentState : DefaultState]?.Invoke(kvp.Value);
				if (nextState is TState ns && !ns.Equals(DefaultState))
				{
					next.Add(kvp.Key, ns);
				}
			}

			(current, next) = (next, current);
			next.Clear();
		}

		public (TCoord min, TCoord max) Bounds()
		{
			if (current.Count == 0)
			{
				return (Vector.Zero<TCoord>(), Vector.Zero<TCoord>());
			}
			else
			{
				TCoord min = current.First().Key;
				TCoord max = min;

				foreach (TCoord coord in current.Keys)
				{
					min = min.MinParts(coord);
					max = max.MaxParts(coord);
				}

				return (min, max);
			}
		}

		public long CountInBounds(Predicate<TState> predicate) =>
			CountInBounds(ConfinementBounds ?? throw new InvalidOperationException($"No {nameof(ConfinementBounds)} defined"), predicate);
		public long CountInBounds((TCoord min,TCoord max) bounds, Predicate<TState> predicate)
		{
			long count = 0;
			var (min, max) = bounds;
			LinkedList<long> taken = new LinkedList<long>();

			void RecursiveCount()
			{
				if (taken.Count < min.Count - 2)
				{
					for (long i = min[min.Count - taken.Count - 1]; i <= max[min.Count - taken.Count - 1]; i++)
					{
						taken.AddFirst(i);
						RecursiveCount();
						taken.RemoveFirst();
					}
				}
				else
				{
					long[] coordArray = new long[min.Count];
					taken.CopyTo(coordArray, 2);
					for (long y = min[1]; y <= max[1]; y++)
					{
						coordArray[1] = y;
						for (long x = min[0]; x <= max[0]; x++)
						{
							coordArray[0] = x;
							TCoord coord = Vector.FromArray<TCoord>(coordArray);
							if (predicate(current.TryGetValue(coord, out TState state) ? state : DefaultState))
							{
								count++;
							}
						}
					}
				}
			}

			if (min.Count > 1)
			{
				RecursiveCount();
			}
			else
			{
				for (long[] coordArray = new [] { min[0] }; coordArray[0] <= max[0]; coordArray[0]++)
				{
					TCoord coord = Vector.FromArray<TCoord>(coordArray);
					if (predicate(current.TryGetValue(coord, out TState state) ? state : DefaultState))
					{
						count++;
					}
				}
			}

			return count;
		}

		public bool Equals(CellularAutomaton<TCoord, TState>? other)
		{
			if (other is null || other.current.Count != current.Count)
			{
				return false;
			}
			else
			{
				foreach (KeyValuePair<TCoord, TState> kvp in current)
				{
					if (!other.current.TryGetValue(kvp.Key, out TState otherValue) || !kvp.Value.Equals(otherValue))
					{
						return false;
					}
				}
				return true;
			}
		}
		public override bool Equals(object? obj) =>
			obj is CellularAutomaton<TCoord, TState> other && Equals(other);
		public override int GetHashCode()
		{
			HashCode hash = new HashCode();
			return hash.ToHashCode();
		}

		public string ToString(IReadOnlyDictionary<TState, char> stateChars)
		{
			StringBuilder sb = new StringBuilder();
			var (min, max) = ConfinementBounds ?? Bounds();
			LinkedList<long> taken = new LinkedList<long>();

			void ToStringRecursive()
			{
				if (taken.Count < min.Count - 2)
				{
					for (long i = min[min.Count - taken.Count - 1]; i <= max[min.Count - taken.Count - 1]; i++)
					{
						taken.AddFirst(i);
						ToStringRecursive();
						taken.RemoveFirst();

						if (i < max[min.Count - taken.Count - 1])
						{
							sb.AppendLine();
						}
					}
				}
				else
				{
					if (min.Count > 2)
					{
						sb.AppendLine($"(…, …, {String.Join(", ", taken)})");
					}
					long[] coordArray = new long[min.Count];
					taken.CopyTo(coordArray, 2);
					for (long y = min[1]; y <= max[1]; y++)
					{
						coordArray[1] = y;
						for (long x = min[0]; x <= max[0]; x++)
						{
							coordArray[0] = x;
							TCoord coord = Vector.FromArray<TCoord>(coordArray);
							sb.Append(stateChars.TryGetValue(current.TryGetValue(coord, out TState state) ? state : DefaultState, out char c) ? c : ' ');
						}
						if (y < max[1])
						{
							sb.AppendLine();
						}
					}
				}
			}

			if (min.Count > 1)
			{
				ToStringRecursive();
			}
			else
			{
				for (long[] coordArray = new [] { min[0] }; coordArray[0] <= max[0]; coordArray[0]++)
				{
					TCoord coord = Vector.FromArray<TCoord>(coordArray);
					sb.Append(stateChars.TryGetValue(current.TryGetValue(coord, out TState state) ? state : DefaultState, out char c) ? c : ' ');
				}
			}

			return sb.ToString();
		}

		public IEnumerator<KeyValuePair<TCoord, TState>> GetEnumerator() =>
			current.GetEnumerator();
		IEnumerator IEnumerable.GetEnumerator() =>
			GetEnumerator();

		public CellularAutomaton<TCoord, TState> Clone() =>
			new CellularAutomaton<TCoord, TState>(this);
		object ICloneable.Clone() =>
			Clone();
	}
}
