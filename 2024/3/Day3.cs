using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;

namespace AoC.Year2024;

[Day(3)]
public partial class Day3
{
    [Part(1)]
    public object Part1(string input) =>
        Part1Regex
            .Matches(input)
            .Select(m => long.Parse(m.Groups[1].Value) * long.Parse(m.Groups[2].Value))
            .Sum();

    [GeneratedRegex(@"mul\((\d+),(\d+)\)")]
    private static partial Regex Part1Regex { get; }

    [Part(2)]
    public object Part2(string input)
    {
        var enable = true;
        var sum = 0L;
        foreach (Match match in Part2Regex.Matches(input))
        {
            if (match.Value is "do()")
            {
                enable = true;
            }
            else if (match.Value is "don't()")
            {
                enable = false;
            }
            else if (enable)
            {
                sum += long.Parse(match.Groups[1].Value) * long.Parse(match.Groups[2].Value);
            }
        }
        return sum;
    }

    [GeneratedRegex(@"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")]
    private static partial Regex Part2Regex { get; }
}
