using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(2)]
public class Day2
{
    [Part(1)]
    public object Part1(string input) =>
        input.Lines().Count(l => IsSafe([.. l.Words().Select(long.Parse)]));

    [Part(2)]
    public object Part2(string input) =>
        input
            .Lines()
            .Count(l =>
            {
                var nums = l.Words().Select(long.Parse).ToList();
                if (IsSafe(nums))
                {
                    return true;
                }
                for (int i = 0; i < nums.Count; i++)
                {
                    var removed = nums[i];
                    nums.RemoveAt(i);
                    if (IsSafe(nums))
                    {
                        return true;
                    }
                    nums.Insert(i, removed);
                }
                return false;
            });

    private static bool IsSafe(IReadOnlyList<long> nums)
    {
        var diff = nums.Zip(nums.Skip(1), (a, b) => b - a).ToArray();
        return (diff[0] < 0 ? diff.All(l => l < 0) : diff.All(l => l > 0))
            && diff.All(l => long.Abs(l) <= 3);
    }
}
