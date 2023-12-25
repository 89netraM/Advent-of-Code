using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Priority_Queue;

namespace AoC.Library
{
	public static class BFS
	{
		private enum UnitDirection
		{
			Unit,
		}

		public static bool Search<TNode>(
			TNode start,
			Func<TNode, IEnumerable<TNode>> getNext,
			Func<TNode, bool> goalCondition,
			out IEnumerable<TNode> path,
			Action<TNode>? between = null,
			IEqualityComparer<TNode>? nodeComparer = null
		) where TNode : notnull =>
			SearchWithPath(
				start,
				getNext,
				goalCondition,
				out path,
				between is not null ? p => between(p.Last()) : null,
				nodeComparer);

		public static bool SearchWithPath<TNode>(
			TNode start,
			Func<TNode, IEnumerable<TNode>> getNext,
			Func<TNode, bool> goalCondition,
			out IEnumerable<TNode> path,
			Action<IEnumerable<TNode>>? between,
			IEqualityComparer<TNode>? nodeComparer = null
		) where TNode : notnull
		{
			bool success = SearchWithPath<TNode, UnitDirection>(
				start,
				n => getNext(n).Select(static next => (UnitDirection.Unit, next, 1L)),
				goalCondition,
				out IEnumerable<(UnitDirection, TNode, long)> internalPath,
				between is not null ? p => between(p.Select(PathSelector)) : null,
				nodeComparer
			);
			path = internalPath.Select(PathSelector);
			return success;

			static TNode PathSelector((UnitDirection, TNode, long) p) => p.Item2;
		}

		public static bool Search<TNode, TDirection>(
			TNode start,
			Func<TNode, IEnumerable<(TDirection, TNode)>> getNext,
			Func<TNode, bool> goalCondition,
			out IEnumerable<(TDirection, TNode)> path,
			Action<TDirection, TNode>? between = null,
			IEqualityComparer<TNode>? nodeComparer = null
		) where TNode : notnull =>
			SearchWithPath(
				start,
				getNext,
				goalCondition,
				out path,
				between is not null ? p => { var (d, n) = p.Last(); between(d, n); } : null,
				nodeComparer);

		public static bool SearchWithPath<TNode, TDirection>(
			TNode start,
			Func<TNode, IEnumerable<(TDirection, TNode)>> getNext,
			Func<TNode, bool> goalCondition,
			out IEnumerable<(TDirection, TNode)> path,
			Action<IEnumerable<(TDirection, TNode)>>? between,
			IEqualityComparer<TNode>? nodeComparer = null
		) where TNode : notnull
		{
			bool success = SearchWithPath<TNode, TDirection>(
				start,
				n => getNext(n).Select(static p => (p.Item1, p.Item2, 1L)),
				goalCondition,
				out IEnumerable<(TDirection, TNode, long)> internalPath,
				between is not null ? p => between(p.Select(PathSelector)) : null,
				nodeComparer
			);
			path = internalPath.Select(PathSelector);
			return success;

			static (TDirection, TNode) PathSelector((TDirection, TNode, long) p) => (p.Item1, p.Item2);
		}

		public static bool Search<TNode>(
			TNode start,
			Func<TNode, IEnumerable<(TNode, long)>> getNext,
			Func<TNode, bool> goalCondition,
			out IEnumerable<(TNode, long)> path,
			Action<TNode, long>? between = null,
			IEqualityComparer<TNode>? nodeComparer = null
		) where TNode : notnull =>
			SearchWithPath(
				start,
				getNext,
				goalCondition,
				out path,
				between is not null ? p => { var (n, c) = p.Last(); between(n, c); } : null,
				nodeComparer);

		public static bool SearchWithPath<TNode>(
			TNode start,
			Func<TNode, IEnumerable<(TNode, long)>> getNext,
			Func<TNode, bool> goalCondition,
			out IEnumerable<(TNode, long)> path,
			Action<IEnumerable<(TNode, long)>>? between,
			IEqualityComparer<TNode>? nodeComparer = null
		) where TNode : notnull
		{
			bool success = SearchWithPath<TNode, UnitDirection>(
				start,
				n => getNext(n).Select(static p => (UnitDirection.Unit, p.Item1, p.Item2)),
				goalCondition,
				out IEnumerable<(UnitDirection, TNode, long)> internalPath,
				between is not null ? p => between(p.Select(PathSelector)) : null,
				nodeComparer
			);
			path = internalPath.Select(PathSelector);
			return success;

			static (TNode, long) PathSelector((UnitDirection, TNode, long) p) => (p.Item2, p.Item3);
		}

		public static bool Search<TNode, TDirection>(
			TNode start,
			Func<TNode, IEnumerable<(TDirection, TNode, long)>> getNext,
			Func<TNode, bool> goalCondition,
			out IEnumerable<(TDirection, TNode, long)> path,
			Action<TDirection, TNode, long>? between = null,
			IEqualityComparer<TNode>? nodeComparer = null
		) where TNode : notnull =>
			SearchWithPath(
				start,
				getNext,
				goalCondition,
				out path,
				between is not null ? p => { var (d, n, c) = p.Last(); between(d, n, c); } : null,
				nodeComparer);

		public static bool SearchWithPath<TNode, TDirection>(
			TNode start,
			Func<TNode, IEnumerable<(TDirection, TNode, long)>> getNext,
			Func<TNode, bool> goalCondition,
			out IEnumerable<(TDirection, TNode, long)> path,
			Action<IEnumerable<(TDirection, TNode, long)>>? between,
			IEqualityComparer<TNode>? nodeComparer = null
		) where TNode : notnull
		{
			IDictionary<TNode, IImmutableList<(TDirection, TNode, long)>> directionTo = new Dictionary<TNode, IImmutableList<(TDirection, TNode, long)>>(nodeComparer);
			directionTo.Add(start, ImmutableList.Create<(TDirection, TNode, long)>());
			IPriorityQueue<TNode, long> toVisit = new SimplePriorityQueue<TNode, long>();
			toVisit.Enqueue(start, 0);

			while (toVisit.Count > 0)
			{
				TNode current = toVisit.Dequeue();
				IImmutableList<(TDirection, TNode, long)> pathToCurrent = directionTo[current];
				if (goalCondition(current))
				{
					path = pathToCurrent;
					return true;
				}
				long cost = pathToCurrent.Count > 0 ? pathToCurrent[pathToCurrent.Count - 1].Item3 : 0L;
				if (between is not null && pathToCurrent.Count > 0)
				{
					between(pathToCurrent);
				}
				foreach (var (direction, next, additionalCost) in getNext(current))
				{
					long nextCost = cost + additionalCost;
					if (!directionTo.TryGetValue(next, out IImmutableList<(TDirection, TNode, long)>? pathToNext) ||
						(pathToNext is not null && pathToNext.Count > 0 && nextCost < pathToNext[pathToNext.Count - 1].Item3))
					{
						directionTo[next] = pathToCurrent.Add((direction, next, nextCost));
						if (toVisit.Contains(next))
						{
							toVisit.UpdatePriority(next, nextCost);
						}
						else
						{
							toVisit.Enqueue(next, nextCost);
						}
					}
				}
			}

			path = Enumerable.Empty<(TDirection, TNode, long)>();
			return false;
		}
	}
}
