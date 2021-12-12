using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Collections.Immutable;
using Priority_Queue;

namespace AoC.Year2021
{
	[Day(12)]
	public class Day12
	{
		[Part(1)]
		public object Part1(string input)
		{
			var map = new Dictionary<string, List<string>>();
			foreach (var (from, to) in input.Lines().Select(l => l.Split('-')).Select(s => (s[0], s[1])))
			{
				if (!map.ContainsKey(from))
				{
					map[from] = new List<string>();
				}
				map[from].Add(to);
				if (!map.ContainsKey(to))
				{
					map[to] = new List<string>();
				}
				map[to].Add(from);
			}

			long FindPath(string from, ImmutableHashSet<string> previouslyVisited)
			{
				HashSet<ImmutableHashSet<string>> pathsTaken = new HashSet<ImmutableHashSet<string>>();
				SimplePriorityQueue<(string, ImmutableHashSet<string>), long> toVisit = new SimplePriorityQueue<(string, ImmutableHashSet<string>), long>();
				toVisit.Enqueue((from, ImmutableHashSet<string>.Empty), 0);

				long count = 0;
				while (toVisit.Count > 0)
				{
					var (current, path) = toVisit.Dequeue();
					long nextCost = path.Count + 1;
					foreach (var next in map[current])
					{
						if (!(previouslyVisited.Contains(next) || pathsTaken.Contains(path.Add(next)) || path.Contains(next)))
						{
							if (next.All(c => Char.IsUpper(c)))
							{
								count += FindPath(next, previouslyVisited.Union(path));
							}
							else if (next == "end")
							{
								count++;
							}
							else
							{
								pathsTaken.Add(path.Add(next));
								toVisit.Enqueue((next, path.Add(next)), nextCost);
							}
						}
					}
				}

				return count;
			}

			return FindPath("start", ImmutableHashSet.Create("start"));
		}

		[Part(2)]
		public object Part2(string input)
		{
			var map = new Dictionary<string, List<string>>();
			foreach (var (from, to) in input.Lines().Select(l => l.Split('-')).Select(s => (s[0], s[1])))
			{
				if (!map.ContainsKey(from))
				{
					map[from] = new List<string>();
				}
				map[from].Add(to);
				if (!map.ContainsKey(to))
				{
					map[to] = new List<string>();
				}
				map[to].Add(from);
			}

			long FindPath(string from, ImmutableHashSet<string> previouslyVisited, string previouslyVisitedTwice, long i = 0)
			{
				HashSet<ImmutableHashSet<string>> pathsTaken = new HashSet<ImmutableHashSet<string>>();
				SimplePriorityQueue<(string, ImmutableHashSet<string>, string), long> toVisit = new SimplePriorityQueue<(string, ImmutableHashSet<string>, string), long>();
				toVisit.Enqueue((from, ImmutableHashSet<string>.Empty, previouslyVisitedTwice), 0);

				long count = 0;
				while (toVisit.Count > 0)
				{
					var (current, path, twice) = toVisit.Dequeue();
					long nextCost = path.Count + 1;
					foreach (var next in map[current])
					{
						if (!(previouslyVisited.Contains(next) || pathsTaken.Contains(path.Add(next)) || path.Contains(next)))
						{
							if (next.All(c => Char.IsUpper(c)))
							{
								count += FindPath(next, previouslyVisited.Union(path), twice, i + 1);
							}
							else if (next == "end")
							{
								count++;
							}
							else
							{
								pathsTaken.Add(path.Add(next));
								toVisit.Enqueue((next, path.Add(next), twice), nextCost);
							}
						}
						else if (twice == String.Empty && next.All(c => Char.IsLower(c)) && next != "start" && next != "end")
						{
							toVisit.Enqueue((next, path.Add(next), next), nextCost);
						}
					}
				}

				return count;
			}

			return FindPath("start", ImmutableHashSet.Create("start"), String.Empty);
		}
	}
}
