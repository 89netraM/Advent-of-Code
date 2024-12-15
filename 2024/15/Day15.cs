// --- Day 15: Warehouse Woes ---
// You appear back inside your own mini submarine! Each Historian drives their mini submarine in a different direction; maybe the Chief has his own submarine down here somewhere as well?
// You look up to see a vast school of lanternfish swimming past you. On closer inspection, they seem quite anxious, so you drive your mini submarine over to see if you can help.
// Because lanternfish populations grow rapidly, they need a lot of food, and that food needs to be stored somewhere. That's why these lanternfish have built elaborate warehouse complexes operated by robots!
// These lanternfish seem so anxious because they have lost control of the robot that operates one of their most important warehouses! It is currently running amok, pushing around boxes in the warehouse with no regard for lanternfish logistics or lanternfish inventory management strategies.
// Right now, none of the lanternfish are brave enough to swim up to an unpredictable robot so they could shut it off. However, if you could anticipate the robot's movements, maybe they could find a safe option.
// The lanternfish already have a map of the warehouse and a list of movements the robot will attempt to make (your puzzle input). The problem is that the movements will sometimes fail as boxes are shifted around, making the actual movements of the robot difficult to predict.
// For example:
// ##########
// #..O..O.O#
// #......O.#
// #.OO..O.O#
// #..O@..O.#
// #O#..O...#
// #O..O..O.#
// #.OO.O.OO#
// #....O...#
// ##########
//
// <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
// vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
// ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
// <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
// ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
// ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
// >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
// <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
// ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
// v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
// As the robot (@) attempts to move, if there are any boxes (O) in the way, the robot will also attempt to push those boxes. However, if this action would cause the robot or a box to move into a wall (#), nothing moves instead, including the robot. The initial positions of these are shown on the map at the top of the document the lanternfish gave you.
// The rest of the document describes the moves (^ for up, v for down, < for left, > for right) that the robot will attempt to make, in order. (The moves form a single giant sequence; they are broken into multiple lines just to make copy-pasting easier. Newlines within the move sequence should be ignored.)
// Here is a smaller example to get started:
// ########
// #..O.O.#
// ##@.O..#
// #...O..#
// #.#.O..#
// #...O..#
// #......#
// ########
//
// <^^>>>vv<v>>v<<
// Were the robot to attempt the given sequence of moves, it would push around the boxes as follows:
// Initial state:
// ########
// #..O.O.#
// ##@.O..#
// #...O..#
// #.#.O..#
// #...O..#
// #......#
// ########
//
// Move <:
// ########
// #..O.O.#
// ##@.O..#
// #...O..#
// #.#.O..#
// #...O..#
// #......#
// ########
//
// Move ^:
// ########
// #.@O.O.#
// ##..O..#
// #...O..#
// #.#.O..#
// #...O..#
// #......#
// ########
//
// Move ^:
// ########
// #.@O.O.#
// ##..O..#
// #...O..#
// #.#.O..#
// #...O..#
// #......#
// ########
//
// Move >:
// ########
// #..@OO.#
// ##..O..#
// #...O..#
// #.#.O..#
// #...O..#
// #......#
// ########
//
// Move >:
// ########
// #...@OO#
// ##..O..#
// #...O..#
// #.#.O..#
// #...O..#
// #......#
// ########
//
// Move >:
// ########
// #...@OO#
// ##..O..#
// #...O..#
// #.#.O..#
// #...O..#
// #......#
// ########
//
// Move v:
// ########
// #....OO#
// ##..@..#
// #...O..#
// #.#.O..#
// #...O..#
// #...O..#
// ########
//
// Move v:
// ########
// #....OO#
// ##..@..#
// #...O..#
// #.#.O..#
// #...O..#
// #...O..#
// ########
//
// Move <:
// ########
// #....OO#
// ##.@...#
// #...O..#
// #.#.O..#
// #...O..#
// #...O..#
// ########
//
// Move v:
// ########
// #....OO#
// ##.....#
// #..@O..#
// #.#.O..#
// #...O..#
// #...O..#
// ########
//
// Move >:
// ########
// #....OO#
// ##.....#
// #...@O.#
// #.#.O..#
// #...O..#
// #...O..#
// ########
//
// Move >:
// ########
// #....OO#
// ##.....#
// #....@O#
// #.#.O..#
// #...O..#
// #...O..#
// ########
//
// Move v:
// ########
// #....OO#
// ##.....#
// #.....O#
// #.#.O@.#
// #...O..#
// #...O..#
// ########
//
// Move <:
// ########
// #....OO#
// ##.....#
// #.....O#
// #.#O@..#
// #...O..#
// #...O..#
// ########
//
// Move <:
// ########
// #....OO#
// ##.....#
// #.....O#
// #.#O@..#
// #...O..#
// #...O..#
// ########
// The larger example has many more moves; after the robot has finished those moves, the warehouse would look like this:
// ##########
// #.O.O.OOO#
// #........#
// #OO......#
// #OO@.....#
// #O#.....O#
// #O.....OO#
// #O.....OO#
// #OO....OO#
// ##########
// The lanternfish use their own custom Goods Positioning System (GPS for short) to track the locations of the boxes. The GPS coordinate of a box is equal to 100 times its distance from the top edge of the map plus its distance from the left edge of the map. (This process does not stop at wall tiles; measure all the way to the edges of the map.)
// So, the box shown below has a distance of 1 from the top edge of the map and 4 from the left edge of the map, resulting in a GPS coordinate of 100 * 1 + 4 = 104.
// #######
// #...O..
// #......
// The lanternfish would like to know the sum of all boxes' GPS coordinates after the robot finishes moving. In the larger example, the sum of all boxes' GPS coordinates is 10092. In the smaller example, the sum is 2028.
// Predict the motion of the robot and boxes in the warehouse. After the robot is finished moving, what is the sum of all boxes' GPS coordinates?
// To begin, get your puzzle input.
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Net.Http.Headers;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using AoC.Library;
using Microsoft.VisualBasic;
using RegExtract;
using static AoC.Library.Functional;

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

        return map.Where(kvp => kvp.Value is Item.Box).Sum(kvp => kvp.Key.Y * 100 + kvp.Key.X);
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

        if (dir.X is 0)
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
            map[pos + dir + dir] = map[pos + dir];
            map.Remove(pos + dir);
            map[pos + dir + dir + Vector2.Right] = map[pos + dir + Vector2.Right];
            map.Remove(pos + dir + Vector2.Right);
            return true;
        }

        return false;
    }

    private static bool BoxPushWide<TMap>(TMap map, Vector2 pos, Vector2 dir)
        where TMap : IDictionary<Vector2, Item>
    {
        if (
            !(
                map.TryGetValue(pos + dir, out var blockingItem)
                && map.TryGetValue(pos + dir + Vector2.Right, out var blockingItem2)
            )
        )
        {
            return true;
        }

        if (blockingItem is Item.Wall || blockingItem2 is Item.Wall)
        {
            return false;
        }

        switch ((blockingItem, blockingItem2))
        {
            case (Item.LeftBox, Item.RightBox):
                if (BoxPushWide(map, pos + dir, dir))
                {
                    Move(pos);
                    return true;
                }
                break;
            case (Item.RightBox, Item.LeftBox):
                var leftMap = new RecorderMap<Item>(map);
                var rightMap = new RecorderMap<Item>(map);
                if (
                    BoxPushWide(leftMap, pos + dir + Vector2.Left, dir)
                    && BoxPushWide(rightMap, pos + dir + Vector2.Right, dir)
                )
                {
                    map.Apply(leftMap, rightMap);
                    Move(pos + Vector2.Left);
                    Move(pos + Vector2.Right);
                    return true;
                }
                break;
            case (_, Item.LeftBox):
                pos += Vector2.Right;
                if (BoxPushWide(map, pos + dir, dir))
                {
                    Move(pos);
                    return true;
                }
                break;
            case (Item.RightBox, _):
                pos += Vector2.Left;
                if (BoxPushWide(map, pos + dir, dir))
                {
                    Move(pos);
                    return true;
                }
                break;
        }

        return false;

        void Move(Vector2 pos)
        {
            map[pos + dir + dir] = map[pos + dir];
            map.Remove(pos + dir);
            map[pos + dir + dir + Vector2.Right] = map[pos + dir + Vector2.Right];
            map.Remove(pos + dir + Vector2.Right);
        }
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

file static class RecorderMapExtensions
{
    public static void Apply<T>(
        this IDictionary<Vector2, T> map,
        RecorderMap<T> left,
        RecorderMap<T> right
    )
    {
        var moved = new HashSet<Vector2>();
        foreach (var (from, dir) in left.Actions)
        {
            map[from + dir] = map[from];
            map.Remove(from);
            moved.Add(from);
        }
        foreach (var (from, dir) in right.Actions)
        {
            if (!moved.Contains(from))
            {
                map[from + dir] = map[from];
                map.Remove(from);
            }
        }
    }
}

file class RecorderMap<T>(IDictionary<Vector2, T> source) : IDictionary<Vector2, T>
{
    private readonly IDictionary<Vector2, T> inner = source.ToDictionary();

    private Vector2 to = Vector2.Zero;
    private readonly List<(Vector2 from, Vector2 dir)> actions = [];

    public IReadOnlyList<(Vector2 from, Vector2 dir)> Actions => actions;

    public T this[Vector2 key]
    {
        get => throw new NotImplementedException();
        set
        {
            to = key;
            inner[key] = value;
        }
    }

    public ICollection<Vector2> Keys => throw new NotImplementedException();

    public ICollection<T> Values => throw new NotImplementedException();

    public int Count => inner.Count;

    public bool IsReadOnly => throw new NotImplementedException();

    public void Add(Vector2 key, T value) => throw new NotImplementedException();

    public void Add(KeyValuePair<Vector2, T> item) => throw new NotImplementedException();

    public void Clear() => throw new NotImplementedException();

    public bool Contains(KeyValuePair<Vector2, T> item) => throw new NotImplementedException();

    public bool ContainsKey(Vector2 key) => throw new NotImplementedException();

    public void CopyTo(KeyValuePair<Vector2, T>[] array, int arrayIndex) =>
        throw new NotImplementedException();

    public IEnumerator<KeyValuePair<Vector2, T>> GetEnumerator() => inner.GetEnumerator();

    public bool Remove(Vector2 key)
    {
        actions.Add((key, to - key));
        return inner.Remove(key);
    }

    public bool Remove(KeyValuePair<Vector2, T> item) => throw new NotImplementedException();

    public bool TryGetValue(Vector2 key, [MaybeNullWhen(false)] out T value) =>
        inner.TryGetValue(key, out value);

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}
