using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2024;

[Day(21)]
public class Day21
{
    private static readonly KeyPadMap NumpadMap =
        new()
        {
            ['A'] = Vector2.Zero,
            ['0'] = Vector2.Left,
            ['1'] = Vector2.Left * 2 + Vector2.Up,
            ['2'] = Vector2.Left + Vector2.Up,
            ['3'] = Vector2.Up,
            ['4'] = Vector2.Left * 2 + Vector2.Up * 2,
            ['5'] = Vector2.Left + Vector2.Up * 2,
            ['6'] = Vector2.Up * 2,
            ['7'] = Vector2.Left * 2 + Vector2.Up * 3,
            ['8'] = Vector2.Left + Vector2.Up * 3,
            ['9'] = Vector2.Up * 3,
        };

    private static readonly KeyPadMap DirectionpadMap =
        new()
        {
            ['A'] = Vector2.Zero,
            ['<'] = Vector2.Left * 2 + Vector2.Down,
            ['v'] = Vector2.Left + Vector2.Down,
            ['>'] = Vector2.Down,
            ['^'] = Vector2.Left,
        };

    private static readonly Dictionary<Vector2, char> DirectionDigit =
        new()
        {
            [Vector2.Up] = '^',
            [Vector2.Left] = '<',
            [Vector2.Right] = '>',
            [Vector2.Down] = 'v',
        };

    [Part(1)]
    public object Part1(string input) =>
        input
            .Lines()
            .Sum(code =>
            {
                var manualMovementLength = FewestKeyPresses(NumpadMap, code, 2);
                var numericValue = long.Parse(code[..^1]);
                return manualMovementLength * numericValue;
            });

    [Part(2)]
    public object Part2(string input) =>
        input
            .Lines()
            .Sum(code =>
            {
                var manualMovementLength = FewestKeyPressesMemo(NumpadMap, code, 25);
                var numericValue = long.Parse(code[..^1]);
                return manualMovementLength * numericValue;
            });

    private static readonly Func<KeyPadMap, string, long, long> FewestKeyPressesMemo = Curry(
        Memoize(Uncurry<KeyPadMap, string, long, long>(FewestKeyPresses))
    );

    private static long FewestKeyPresses(KeyPadMap map, string code, long level)
    {
        var current = Vector2.Zero;
        var length = 0L;
        foreach (var c in code)
        {
            var next = map[c];
            length += level is 0
                ? GetAllPaths(map, current, next).Min(path => path.Length)
                : GetAllPaths(map, current, next)
                    .Min(path => FewestKeyPressesMemo(DirectionpadMap, path, level - 1));
            current = next;
        }
        return length;
    }

    private static IReadOnlyList<string> GetAllPaths(KeyPadMap map, Vector2 start, Vector2 end)
    {
        if (start == end)
        {
            return ["A"];
        }

        var allPaths = new List<string>();
        var distanceTo = new Dictionary<Vector2, long> { [start] = 0 };
        var toVisit = new Queue<(Vector2 pos, ImmutableList<char> path)>();
        toVisit.Enqueue((start, []));

        while (toVisit.TryDequeue(out var current))
        {
            if (current.pos == end)
            {
                allPaths.Add(string.Concat(current.path.Add('A')));
                continue;
            }

            foreach (var (vector, direction) in DirectionDigit)
            {
                var nextPos = current.pos + vector;
                var nextPath = current.path.Add(direction);
                if (
                    map.ContainsPosition(nextPos)
                    && (
                        !distanceTo.TryGetValue(nextPos, out var distance)
                        || distance >= nextPath.Count
                    )
                )
                {
                    toVisit.Enqueue((nextPos, nextPath));
                    distanceTo[nextPos] = nextPath.Count;
                }
            }
        }

        return allPaths;
    }

    private class KeyPadMap
    {
        private readonly Dictionary<Vector2, char> toDigit = [];
        private readonly Dictionary<char, Vector2> toPosition = [];

        public Vector2 this[char d]
        {
            get => toPosition[d];
            set
            {
                toPosition[d] = value;
                toDigit[value] = d;
            }
        }

        public bool ContainsPosition(Vector2 position) => toDigit.ContainsKey(position);
    }
}
