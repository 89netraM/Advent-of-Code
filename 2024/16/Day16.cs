using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(16)]
public class Day16
{
    [Part(1)]
    public object Part1(string input)
    {
        var map = input.ToMap();
        var start = map.First(kvp => kvp.Value is 'S').Key;
        var goal = map.First(kvp => kvp.Value is 'E').Key;
        BFS.SearchWithPath<Vector2, Vector2>(
            start + Vector2.Left,
            (dT, n) =>
                Vector2
                    .Zero.NeighborsVonNeumann()
                    .Where(d => map.TryGetValue(n + d, out var t) && t is not '#')
                    .Select(d => (d, n + d, dT == default || dT == d ? 1L : 1001L)),
            n => n == goal,
            out var path
        );
        return path.Last().Item3 - 1;
    }

    // [Part(2)]
    // public object Part2(string input)
    // {
    //     var map = input.ToMap();
    //     var start = map.First(kvp => kvp.Value is 'S').Key;
    //     var end = map.First(kvp => kvp.Value is 'E').Key;
    //     var positions = map.Where(kvp => kvp.Value is not '#').Select(kvp => kvp.Key).ToHashSet();

    //     var directCost = new Dictionary<(Node, Node), long>();
    //     foreach (var position in positions)
    //     {
    //         foreach (var fromDirection in Vector.DirectionsVonNeumann<Vector2>())
    //         {
    //             var from = new Node(position, fromDirection);
    //             foreach (var toDirection in Vector.DirectionsVonNeumann<Vector2>())
    //             {
    //                 if (positions.Contains(position + toDirection)) { }
    //             }
    //         }
    //     }

    //     BFS.SearchWithPath<Vector2, Vector2>(
    //         start + Vector2.Left,
    //         (dT, n) =>
    //             Vector2
    //                 .Zero.NeighborsVonNeumann()
    //                 .Where(d => map.TryGetValue(n + d, out var t) && t is not '#')
    //                 .Select(d => (d, n + d, dT == default || dT == d ? 1L : 1001L)),
    //         n => n == end,
    //         out var path
    //     );
    //     var cheapest = (path.Last().Item3 - 1).Dump();

    //     // var count = 0L;
    //     // foreach (var node in nodes)
    //     // {
    //     //     foreach (var direction in Vector.DirectionsVonNeumann<Vector2>())
    //     //     {
    //     //         if (IsOnCheapestPath((node, direction)))
    //     //         {
    //     //             count++;
    //     //             break;
    //     //         }
    //     //     }
    //     //     Console.WriteLine(count);
    //     // }
    //     // return count;
    //     var visited = 0L;
    //     return positions
    //         .AsParallel()
    //         .Count(n =>
    //         {
    //             var yes = Vector.DirectionsVonNeumann<Vector2>().Any(d => IsOnCheapestPath((n, d)));
    //             Interlocked.Increment(ref visited);
    //             Console.WriteLine(visited / (double)positions.Count);
    //             return yes;
    //         });

    //     bool IsOnCheapestPath((Vector2 node, Vector2 direction) position)
    //     {
    //         if (
    //             CheapestToEnd(position, out var costToEnd)
    //             && costToEnd <= cheapest
    //             && CheapestFromStart(position, out var costToStart)
    //         )
    //         {
    //             return costToEnd + costToStart == cheapest;
    //         }
    //         return false;
    //     }

    //     bool CheapestToEnd((Vector2 node, Vector2 direction) from, out long cost) =>
    //         Search(from, n => n.node == end, out cost);

    //     bool CheapestFromStart((Vector2 node, Vector2 direction) target, out long cost) =>
    //         Search((start, Vector2.Right), n => n == target, out cost);

    //     bool Search(
    //         (Vector2 node, Vector2 direction) start,
    //         Func<(Vector2 node, Vector2 direction), bool> goalCondition,
    //         out long cost
    //     )
    //     {
    //         try
    //         {
    //             var success = BFS.Search(
    //                 start,
    //                 n => fromTo[n].Select(n => (n.Key, n.Value)),
    //                 goalCondition,
    //                 out var p,
    //                 (_, c) =>
    //                 {
    //                     if (c > cheapest)
    //                     {
    //                         throw new Exception();
    //                     }
    //                 }
    //             );
    //             if (success)
    //             {
    //                 var path = p.ToArray();
    //                 cost = path is [.., var last] ? last.Item2 : 0;
    //                 return true;
    //             }
    //             cost = 0;
    //             return false;
    //         }
    //         catch
    //         {
    //             cost = 0;
    //             return false;
    //         }
    //     }
    // }

    // private static Dictionary<(Node, Node), long> Search(
    //     IReadOnlyDictionary<Vector2, char> map,
    //     Vector2 goal,
    //     ImmutableList<Vector2> path,
    //     Vector2 direction,
    //     long cost,
    //     long maxCost
    // )
    // {
    //     IDictionary<TNode, IImmutableList<(TDirection, TNode, long)>> directionTo = new Dictionary<
    //         TNode,
    //         IImmutableList<(TDirection, TNode, long)>
    //     >(nodeComparer);
    //     directionTo.Add(start, ImmutableList.Create<(TDirection, TNode, long)>());
    //     IPriorityQueue<TNode, long> toVisit = new SimplePriorityQueue<TNode, long>();
    //     toVisit.Enqueue(start, 0);

    //     while (toVisit.Count > 0)
    //     {
    //         TNode current = toVisit.Dequeue();
    //         IImmutableList<(TDirection, TNode, long)> pathToCurrent = directionTo[current];
    //         if (goalCondition(current))
    //         {
    //             path = pathToCurrent;
    //             return true;
    //         }
    //         long cost = pathToCurrent.Count > 0 ? pathToCurrent[pathToCurrent.Count - 1].Item3 : 0L;
    //         if (between is not null && pathToCurrent.Count > 0)
    //         {
    //             between(pathToCurrent);
    //         }
    //         TDirection? directionToCurrent = pathToCurrent is [.., (var d, _, _)] ? d : default;
    //         foreach (var (direction, next, additionalCost) in getNext(directionToCurrent, current))
    //         {
    //             long nextCost = cost + additionalCost;
    //             if (
    //                 !directionTo.TryGetValue(
    //                     next,
    //                     out IImmutableList<(TDirection, TNode, long)>? pathToNext
    //                 )
    //                 || (
    //                     pathToNext is not null
    //                     && pathToNext.Count > 0
    //                     && nextCost < pathToNext[pathToNext.Count - 1].Item3
    //                 )
    //             )
    //             {
    //                 directionTo[next] = pathToCurrent.Add((direction, next, nextCost));
    //                 if (toVisit.Contains(next))
    //                 {
    //                     toVisit.UpdatePriority(next, nextCost);
    //                 }
    //                 else
    //                 {
    //                     toVisit.Enqueue(next, nextCost);
    //                 }
    //             }
    //         }
    //     }
    // }

    // private record Node(Vector2 Position, Vector2 Direction);
}
