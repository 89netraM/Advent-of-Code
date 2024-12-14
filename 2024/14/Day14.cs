// --- Day 14: Restroom Redoubt ---
// One of The Historians needs to use the bathroom; fortunately, you know there's a bathroom near an unvisited location on their list, and so you're all quickly teleported directly to the lobby of Easter Bunny Headquarters.
// Unfortunately, EBHQ seems to have "improved" bathroom security again after your last visit. The area outside the bathroom is swarming with robots!
// To get The Historian safely to the bathroom, you'll need a way to predict where the robots will be in the future. Fortunately, they all seem to be moving on the tile floor in predictable straight lines.
// You make a list (your puzzle input) of all of the robots' current positions (p) and velocities (v), one robot per line. For example:
// p=0,4 v=3,-3
// p=6,3 v=-1,-3
// p=10,3 v=-1,2
// p=2,0 v=2,-1
// p=0,0 v=1,3
// p=3,0 v=-2,-2
// p=7,6 v=-1,-3
// p=3,0 v=-1,-2
// p=9,3 v=2,3
// p=7,3 v=-1,2
// p=2,4 v=2,-3
// p=9,5 v=-3,-3
// Each robot's position is given as p=x,y where x represents the number of tiles the robot is from the left wall and y represents the number of tiles from the top wall (when viewed from above). So, a position of p=0,0 means the robot is all the way in the top-left corner.
// Each robot's velocity is given as v=x,y where x and y are given in tiles per second. Positive x means the robot is moving to the right, and positive y means the robot is moving down. So, a velocity of v=1,-2 means that each second, the robot moves 1 tile to the right and 2 tiles up.
// The robots outside the actual bathroom are in a space which is 101 tiles wide and 103 tiles tall (when viewed from above). However, in this example, the robots are in a space which is only 11 tiles wide and 7 tiles tall.
// The robots are good at navigating over/under each other (due to a combination of springs, extendable legs, and quadcopters), so they can share the same tile and don't interact with each other. Visually, the number of robots on each tile in this example looks like this:
// 1.12.......
// ...........
// ...........
// ......11.11
// 1.1........
// .........1.
// .......1...
// These robots have a unique feature for maximum bathroom security: they can teleport. When a robot would run into an edge of the space they're in, they instead teleport to the other side, effectively wrapping around the edges. Here is what robot p=2,4 v=2,-3 does for the first few seconds:
// Initial state:
// ...........
// ...........
// ...........
// ...........
// ..1........
// ...........
// ...........
//
// After 1 second:
// ...........
// ....1......
// ...........
// ...........
// ...........
// ...........
// ...........
//
// After 2 seconds:
// ...........
// ...........
// ...........
// ...........
// ...........
// ......1....
// ...........
//
// After 3 seconds:
// ...........
// ...........
// ........1..
// ...........
// ...........
// ...........
// ...........
//
// After 4 seconds:
// ...........
// ...........
// ...........
// ...........
// ...........
// ...........
// ..........1
//
// After 5 seconds:
// ...........
// ...........
// ...........
// .1.........
// ...........
// ...........
// ...........
// The Historian can't wait much longer, so you don't have to simulate the robots for very long. Where will the robots be after 100 seconds?
// In the above example, the number of robots on each tile after 100 seconds has elapsed looks like this:
// ......2..1.
// ...........
// 1..........
// .11........
// .....1.....
// ...12......
// .1....1....
// To determine the safest area, count the number of robots in each quadrant after 100 seconds. Robots that are exactly in the middle (horizontally or vertically) don't count as being in any quadrant, so the only relevant robots are:
// ..... 2..1.
// ..... .....
// 1.... .....
//
// ..... .....
// ...12 .....
// .1... 1....
// In this example, the quadrants contain 1, 3, 4, and 1 robot. Multiplying these together gives a total safety factor of 12.
// Predict the motion of the robots in your list within a space which is 101 tiles wide and 103 tiles tall. What will the safety factor be after exactly 100 seconds have elapsed?
// To begin, get your puzzle input.
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using AoC.Library;
using RegExtract;
using static AoC.Library.Functional;

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
        var center = max / 2;
        var robots = input
            .Lines()
            .Extract<Robot>(@"p=((-?\d+),(-?\d+))\sv=((-?\d+),(-?\d+))")
            .ToArray();
        var steps = Enumerable
            .Range(0, 100_001)
            .AsParallel()
            .MinBy(s => ChristmasTreeDistance(PositionsAfter(robots, s)));
            // .First(s => Corners(PositionsAfter(robots, s)));

        var positions = PositionsAfter(robots, steps);

        Console.WriteLine($"{steps} ({ChristmasTreeDistance(positions)})\e[K");
        var sb = new StringBuilder();
        for (var y = 0L; y < max.Y; y++)
        {
            for (var x = 0L; x < max.X; x++)
            {
                var pos = new Vector2(x, y);
                var count = positions.Count(p => p == pos);
                sb.Append(
                    count is 0
                        ? "."
                        : count is < 10
                            ? count.ToString()
                            : "X"
                );
            }
            sb.AppendLine();
        }
        Console.Write(sb);

        return steps;

        // var steps = 0L;
        // while (true)
        // {
        //     Console.WriteLine($"{steps} ({MedianDistance()})\e[K");
        //     var sb = new StringBuilder();
        //     for (var y = 0L; y < max.Y; y++)
        //     {
        //         for (var x = 0L; x < max.X; x++)
        //         {
        //             var pos = new Vector2(x, y);
        //             if (pos.X == center.X || pos.Y == center.Y)
        //             {
        //                 sb.Append(' ');
        //                 continue;
        //             }
        //             var count = robots.Count(r => r.Position == pos);
        //             sb.Append(
        //                 count is 0
        //                     ? "."
        //                     : count is < 10
        //                         ? count.ToString()
        //                         : "X"
        //             );
        //         }
        //         sb.AppendLine();
        //     }
        //     Console.Write(sb);
        //     readKey:
        //     switch (Console.ReadKey(intercept: true))
        //     {
        //         case { Modifiers: ConsoleModifiers.Control | ConsoleModifiers.Shift, Key: ConsoleKey.LeftArrow }:
        //             Search(-1);
        //             break;
        //         case { Modifiers: ConsoleModifiers.Control, Key: ConsoleKey.LeftArrow }:
        //             steps -= 100;
        //             StepRobots(-100);
        //             break;
        //         case { Modifiers: ConsoleModifiers.Shift, Key: ConsoleKey.LeftArrow }:
        //             steps -= 10;
        //             StepRobots(-10);
        //             break;
        //         case { Key: ConsoleKey.LeftArrow }:
        //             steps--;
        //             StepRobots(-1);
        //             break;
        //         case { Modifiers: ConsoleModifiers.Control | ConsoleModifiers.Shift, Key: ConsoleKey.RightArrow }:
        //             Search(1);
        //             break;
        //         case { Modifiers: ConsoleModifiers.Control, Key: ConsoleKey.RightArrow }:
        //             steps += 100;
        //             StepRobots(100);
        //             break;
        //         case { Modifiers: ConsoleModifiers.Shift, Key: ConsoleKey.RightArrow }:
        //             steps += 10;
        //             StepRobots(10);
        //             break;
        //         case { Key: ConsoleKey.RightArrow }:
        //             steps++;
        //             StepRobots(1);
        //             break;
        //         case { Key: ConsoleKey.Enter }:
        //             return steps;
        //         default:
        //             goto readKey;
        //     }
        //     Console.CursorTop -= (int)max.Y + 1;
        // }

        // void Search(long dir)
        // {
        //     do
        //     {
        //         steps += dir;
        //         StepRobots(dir);
        //     }
        //     while (MedianDistance() > max.Y * 0.35d);
        // }

        IEnumerable<Vector2> PositionsAfter(IEnumerable<Robot> robots, long steps) =>
            robots.Select(r => new Vector2(
                MathM.Mod(r.Position.X + r.Velocity.X * steps, max.X),
                MathM.Mod(r.Position.Y + r.Velocity.Y * steps, max.X)
            ));

        static double ChristmasTreeDistance(IEnumerable<Vector2> positions)
        {
            var poses = positions.ToArray();
            var averagePos = positions.Aggregate((a, b) => a + b) / poses.Length;
            var distances = new List<double>();
            for (int i = 0; i < poses.Length; i++)
            {
                distances.Add(poses[i].Distance(averagePos));
            }
            distances.Sort();
            return distances[distances.Count / 2];
        }

        bool Corners(IEnumerable<Vector2> positions)
        {
            var poses = positions.ToHashSet();

            for (long y = 0; y <= max.Y; y++)
            for (long x = 0; x <= max.X; x++)
            {
                var pos = new Vector2(x, y);
                if (!(poses.Contains(pos) && poses.Contains(pos + Vector2.Right) && poses.Contains(pos + Vector2.Down)))
                {
                    continue;
                }

                for (var right = new Vector2(x + 1, y); right.X <= max.X; right += Vector2.Right)
                {
                    if (!poses.Contains(right))
                    {
                        goto end;
                    }

                    if (right.X > pos.X + 5)
                    {
                        return true;
                    }
                }
                end: /* NOP */;
            }

            return false;
        }
    }

    record Robot(Vector2 Position, Vector2 Velocity);
}
