using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2024;

[Day(14)]
public class Day14
{
    [Part(1)]
    public object Part1(string input)
    {
        var max = new Vector2(101, 103);
        var center = max / 2;
        return input
            .Lines()
            .Extract<Robot>(@"p=((-?\d+),(-?\d+))\sv=((-?\d+),(-?\d+))")
            .Select(r => r with { Position = r.Position + r.Velocity * 100 })
            .Select(r =>
                r with
                {
                    Position = new(MathM.Mod(r.Position.X, max.X), MathM.Mod(r.Position.Y, max.Y)),
                }
            )
            .Select(r => r.Position)
            .Where(r => r.X != center.X && r.Y != center.Y)
            .GroupBy(r => (r.X < center.X, r.Y < center.Y))
            .Product(g => g.Count());
    }

    [Part(2)]
    public object Part2(string input)
    {
        var max = new Vector2(101, 103);
        var robots = input
            .Lines()
            .Extract<Robot>(@"p=((-?\d+),(-?\d+))\sv=((-?\d+),(-?\d+))")
            .ToArray();
        return Enumerable
            .Range(0, 10_001)
            .First(s => Line(PositionsAfter(robots, s)));

        IEnumerable<Vector2> PositionsAfter(IEnumerable<Robot> robots, long steps) =>
            robots.Select(r => new Vector2(
                MathM.Mod(r.Position.X + r.Velocity.X * steps, max.X),
                MathM.Mod(r.Position.Y + r.Velocity.Y * steps, max.Y)
            ));

        static bool Line(IEnumerable<Vector2> positions)
        {
            var poses = positions.ToHashSet();
            foreach (var pos in poses)
            {
                if (Enumerable.Range(1, 9).All(o => poses.Contains(pos + new Vector2(0, o))))
                {
                    return true;
                }
            }
            return false;
        }
    }

    record Robot(Vector2 Position, Vector2 Velocity);
}
