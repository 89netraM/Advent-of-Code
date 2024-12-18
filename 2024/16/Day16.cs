using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using Priority_Queue;

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

    [Part(2)]
    public long Part2(string input)
    {
        var map = input.ToMap();
        var start = map.First(kvp => kvp.Value is 'S').Key;
        var startNode = new Node(start, Vector2.Right);
        var end = map.First(kvp => kvp.Value is 'E').Key;
        var openPositions = map.Where(kvp => kvp.Value is not '#')
            .Select(kvp => kvp.Key)
            .ToHashSet();

        var fromStartTo = SearchFromStart(openPositions, startNode);
        var toEndFrom = SearchToEnd(openPositions, end);
        var cheapestRoute = toEndFrom[startNode];

        var goodSeatingPositions = new HashSet<Vector2>();
        foreach (var position in openPositions)
        {
            foreach (var direction in Vector.DirectionsVonNeumann<Vector2>())
            {
                var node = new Node(position, direction);
                if (fromStartTo[node] + toEndFrom[node] == cheapestRoute)
                {
                    goodSeatingPositions.Add(position);
                    break;
                }
            }
        }
        return goodSeatingPositions.Count;
    }

    private static Dictionary<Node, long> SearchFromStart(
        HashSet<Vector2> openPositions,
        Node startNode
    )
    {
        var fromStartTo = new Dictionary<Node, long> { [startNode] = 0 };
        var toVisit = new SimplePriorityQueue<Node, long>();
        toVisit.Enqueue(startNode, 0);
        Search(
            openPositions,
            fromStartTo,
            toVisit,
            node => node with { Position = node.Position + node.Direction }
        );
        return fromStartTo;
    }

    private static Dictionary<Node, long> SearchToEnd(
        HashSet<Vector2> openPositions,
        Vector2 endPosition
    )
    {
        var toEndFrom = Vector
            .DirectionsVonNeumann<Vector2>()
            .Select(d => new Node(endPosition, d))
            .ToDictionary(n => n, _ => 0L);
        var toVisit = new SimplePriorityQueue<Node, long>();
        foreach (var (node, cost) in toEndFrom)
        {
            toVisit.Enqueue(node, cost);
        }
        Search(
            openPositions,
            toEndFrom,
            toVisit,
            node => node with { Position = node.Position - node.Direction }
        );
        return toEndFrom;
    }

    private static void Search(
        HashSet<Vector2> openPositions,
        Dictionary<Node, long> cost,
        SimplePriorityQueue<Node, long> toVisit,
        Func<Node, Node> move
    )
    {
        while (toVisit.TryDequeue(out var currentNode))
        {
            var currentCost = cost[currentNode];

            var turnCost = currentCost + 1000;
            var turnNodes = Vector
                .DirectionsVonNeumann<Vector2>()
                .Where(d => d != currentNode.Direction)
                .Select(d => currentNode with { Direction = d })
                .Where(n => !cost.TryGetValue(n, out var oldCost) || turnCost < oldCost);
            foreach (var turnNode in turnNodes)
            {
                cost[turnNode] = turnCost;
                if (!toVisit.TryUpdatePriority(turnNode, turnCost))
                {
                    toVisit.Enqueue(turnNode, turnCost);
                }
            }

            var moveCost = currentCost + 1;
            var moveNode = move(currentNode);
            if (
                openPositions.Contains(moveNode.Position)
                && (!cost.TryGetValue(moveNode, out var oldCost) || moveCost < oldCost)
            )
            {
                cost[moveNode] = moveCost;
                if (!toVisit.TryUpdatePriority(moveNode, turnCost))
                {
                    toVisit.Enqueue(moveNode, turnCost);
                }
            }
        }
    }

    private record Node(Vector2 Position, Vector2 Direction);
}
