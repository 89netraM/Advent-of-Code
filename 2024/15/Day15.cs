using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using AoC.Library;

namespace AoC.Year2024;

[Day(15)]
public class Day15
{
    [Part(1)]
    public object Part1(string input)
    {
        if (input.Paragraphs() is not [var inputMap, var inputPath])
        {
            throw new ArgumentException("Invalid input");
        }

        var charMap = inputMap.ToMap();
        var robotPos = charMap.First(kvp => kvp.Value is '@').Key;
        var map = charMap
            .Where(kvp => kvp.Value is 'O' or '#')
            .ToDictionary(
                kvp => kvp.Key,
                kvp =>
                    kvp.Value switch
                    {
                        'O' => Item.Box,
                        '#' => Item.Wall,
                        _ => throw new ArgumentException("Unrecognized map tile"),
                    }
            );

        var path = inputPath
            .Where(c => c is '^' or '<' or '>' or 'v')
            .Select(c =>
                c switch
                {
                    '^' => Vector2.Up,
                    '<' => Vector2.Left,
                    '>' => Vector2.Right,
                    'v' => Vector2.Down,
                    _ => throw new ArgumentException($"Unrecognized direction ({c})"),
                }
            );

        foreach (var direction in path)
        {
            if (Push(map, robotPos, direction))
            {
                robotPos += direction;
            }
        }

        return map.Where(kvp => kvp.Value is Item.Box).Sum(kvp => kvp.Key.Y * 100 + kvp.Key.X);
    }

    private static bool Push(Dictionary<Vector2, Item> map, Vector2 pos, Vector2 dir)
    {
        if (!map.TryGetValue(pos + dir, out var blockingItem))
        {
            return true;
        }

        if (blockingItem is Item.Wall)
        {
            return false;
        }

        if (Push(map, pos + dir, dir))
        {
            map[pos + dir + dir] = blockingItem;
            map.Remove(pos + dir);
            return true;
        }

        return false;
    }

    [Part(2)]
    public object Part2(string input)
    {
        if (input.Paragraphs() is not [var inputMap, var inputPath])
        {
            throw new ArgumentException("Invalid input");
        }

        var charMap = inputMap.ToMap();
        var robotPos = charMap
            .First(kvp => kvp.Value is '@')
            .Key.Let(p => new Vector2(p.X * 2, p.Y));
        var map = charMap
            .Where(kvp => kvp.Value is 'O' or '#')
            .Select(kvp => new KeyValuePair<Vector2, Item>(
                kvp.Key.Let(p => new Vector2(p.X * 2, p.Y)),
                kvp.Value switch
                {
                    'O' => Item.Box,
                    '#' => Item.Wall,
                    _ => throw new ArgumentException($"Unrecognized map tile {kvp.Value}"),
                }
            ))
            .SelectMany<KeyValuePair<Vector2, Item>, KeyValuePair<Vector2, Item>>(kvp =>
                kvp.Value switch
                {
                    Item.Wall
                        =>
                        [
                            kvp,
                            new KeyValuePair<Vector2, Item>(kvp.Key + Vector2.Right, kvp.Value)
                        ],
                    Item.Box
                        =>
                        [
                            new KeyValuePair<Vector2, Item>(kvp.Key, Item.LeftBox),
                            new KeyValuePair<Vector2, Item>(kvp.Key + Vector2.Right, Item.RightBox)
                        ],
                    _ => throw new ArgumentException($"Unrecognized map tile {kvp.Value}"),
                }
            )
            .ToDictionary();

        var path = inputPath
            .Where(c => c is '^' or '<' or '>' or 'v')
            .Select(c =>
                c switch
                {
                    '^' => Vector2.Up,
                    '<' => Vector2.Left,
                    '>' => Vector2.Right,
                    'v' => Vector2.Down,
                    _ => throw new ArgumentException($"Unrecognized direction ({c})"),
                }
            );

        foreach (var direction in path)
        {
            if (RobotPushWide(map, robotPos, direction))
            {
                robotPos += direction;
            }
        }

        return map.Where(kvp => kvp.Value is Item.LeftBox).Sum(kvp => kvp.Key.Y * 100 + kvp.Key.X);
    }

    private static bool RobotPushWide(Dictionary<Vector2, Item> map, Vector2 pos, Vector2 dir)
    {
        if (!map.TryGetValue(pos + dir, out var blockingItem))
        {
            return true;
        }

        if (blockingItem is Item.Wall)
        {
            return false;
        }

        if (dir.Y is 0)
        {
            if (Push(map, pos + dir, dir))
            {
                map[pos + dir + dir] = blockingItem;
                map.Remove(pos + dir);
                return true;
            }

            return false;
        }

        if (blockingItem is Item.RightBox)
        {
            pos += Vector2.Left;
        }

        if (BoxPushWide(map, pos + dir, dir))
        {
            if (map.TryGetValue(pos + dir, out var itemToPush))
            {
                map[pos + dir + dir] = itemToPush;
                map.Remove(pos + dir);
            }
            if (map.TryGetValue(pos + dir + Vector2.Right, out itemToPush))
            {
                map[pos + dir + dir + Vector2.Right] = itemToPush;
                map.Remove(pos + dir + Vector2.Right);
            }
            return true;
        }

        return false;
    }

    private static bool BoxPushWide<TMap>(TMap map, Vector2 startPos, Vector2 dir)
        where TMap : IDictionary<Vector2, Item>
    {
        var queue = new Queue<Vector2>([startPos]);
        var pushes = new HashSet<Vector2>();

        while (queue.TryDequeue(out var from))
        {
            if (
                !map.TryGetValue(from + dir, out var leftItem)
                & !map.TryGetValue(from + dir + Vector2.Right, out var rightItem)
            )
            {
                continue;
            }

            if (leftItem is Item.Wall || rightItem is Item.Wall)
            {
                return false;
            }

            switch ((leftItem, rightItem))
            {
                case (Item.LeftBox, Item.RightBox):
                    queue.Enqueue(from + dir);
                    pushes.Add(from + dir);
                    pushes.Add(from + Vector2.Right + dir);
                    break;
                case (Item.RightBox, Item.LeftBox):
                    queue.Enqueue(from + Vector2.Left + dir);
                    queue.Enqueue(from + Vector2.Right + dir);
                    pushes.Add(from + Vector2.Left + dir);
                    pushes.Add(from + dir);
                    pushes.Add(from + Vector2.Right + dir);
                    pushes.Add(from + Vector2.Right * 2 + dir);
                    break;
                case (Item.RightBox, _):
                    queue.Enqueue(from + Vector2.Left + dir);
                    pushes.Add(from + Vector2.Left + dir);
                    pushes.Add(from + dir);
                    break;
                case (_, Item.LeftBox):
                    queue.Enqueue(from + Vector2.Right + dir);
                    pushes.Add(from + Vector2.Right + dir);
                    pushes.Add(from + Vector2.Right * 2 + dir);
                    break;
                case var pair:
                    throw new Exception($"Aaaaa! {pair}");
            }
        }

        foreach (var push in pushes.OrderBy(v => v.Y * -dir.Y))
        {
            map[push + dir] = map[push];
            map.Remove(push);
        }

        return true;
    }

    private static string ToString(Dictionary<Vector2, Item> map, Vector2 robotPos)
    {
        var min = map.Keys.Aggregate((a, b) => a.MinParts(b));
        var max = map.Keys.Aggregate((a, b) => a.MaxParts(b));
        var sb = new StringBuilder();
        for (var y = min.Y; y <= max.Y; y++)
        {
            for (var x = min.X; x <= max.X; x++)
            {
                var pos = new Vector2(x, y);
                if (pos == robotPos)
                {
                    sb.Append('@');
                }
                else if (map.TryGetValue(pos, out var item))
                {
                    sb.Append(
                        item switch
                        {
                            Item.Box => 'O',
                            Item.LeftBox => '[',
                            Item.RightBox => ']',
                            Item.Wall => '#',
                            _ => throw new Exception(),
                        }
                    );
                }
                else
                {
                    sb.Append('.');
                }
            }
            if (y < max.Y)
            {
                sb.AppendLine();
            }
        }
        return sb.ToString();
    }

    enum Item
    {
        Empty,
        Box,
        LeftBox,
        RightBox,
        Wall,
    }
}
