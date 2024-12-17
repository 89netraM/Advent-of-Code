using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2024;

[Day(17)]
public class Day17
{
    [Part(1)]
    public object Part1(string input)
    {
        var registers = input
            .Paragraphs()[0]
            .Lines()
            .Extract<(char, long)>(@"Register (.): (\d+)")
            .ToDictionary(p => p.Item1, p => p.Item2);
        var program = input
            .Paragraphs()[1]
            .Extract<string>(@"Program: ((?:\d,?)+)")
            .Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Select(long.Parse)
            .ToArray();

        var a = registers['A'];

        var output = new List<long>();

        long pc = 0;
        while (pc < program.Length)
        {
            switch (program[pc])
            {
                case 0:
                    var combo = ComboOperand(program[++pc]);
                    registers['A'] = registers['A'] / (1L << (int)combo);
                    pc++;
                    break;
                case 1:
                    registers['B'] = registers['B'] ^ program[++pc];
                    pc++;
                    break;
                case 2:
                    registers['B'] = MathM.Mod(ComboOperand(program[++pc]), 8);
                    pc++;
                    break;
                case 3:
                    if (registers['A'] is not 0)
                    {
                        pc = program[++pc];
                    }
                    else
                    {
                        pc += 2;
                    }
                    break;
                case 4:
                    registers['B'] = registers['B'] ^ registers['C'];
                    pc += 2;
                    break;
                case 5:
                    output.Add(MathM.Mod(ComboOperand(program[++pc]), 8));
                    pc++;
                    break;
                case 6:
                    combo = ComboOperand(program[++pc]);
                    registers['B'] = registers['A'] / (1L << (int)combo);
                    pc++;
                    break;
                case 7:
                    combo = ComboOperand(program[++pc]);
                    registers['C'] = registers['A'] / (1L << (int)combo);
                    pc++;
                    break;
            }
        }

        return string.Join(',', output);

        long ComboOperand(long value) =>
            value switch
            {
                0 or 1 or 2 or 3 => value,
                4 => registers['A'],
                5 => registers['B'],
                6 => registers['C'],
                7 => throw new InvalidOperationException("Encountered combo operand 7"),
                _
                    => throw new InvalidOperationException(
                        $"Encountered unknown combo operand {value}"
                    )
            };
    }

    [Part(2)]
    public object Part2(string input)
    {
        var program = input
            .Paragraphs()[1]
            .Extract<string>(@"Program: ((?:\d,?)+)")
            .Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Select(long.Parse)
            .Reverse()
            .ToArray();

        return Search(1, 0);

        long? Search(long a, int matching)
        {
            for (; true; a++)
            {
                var output = RunProgramCompiled(a).Reverse().ToArray();
                if (output.Length > program.Length)
                {
                    return null;
                }
                if (!output.Take(matching).SequenceEqual(program.Take(matching)))
                {
                    return null;
                }
                if (output.SequenceEqual(program.Take(output.Length)))
                {
                    if (output.Length == program.Length)
                    {
                        return a;
                    }
                    if (Search(a << 3, output.Length) is long result)
                    {
                        return result;
                    }
                }
            }
        }
    }

    private static IEnumerable<long> RunProgramCompiled(long a)
    {
        long b = 0;
        long c = 0;

        while (a is not 0)
        {
            b = (a & 0b111) ^ 3;
            c = a / (1L << (int)b);
            b = b ^ 5 ^ c;
            a /= 8;
            yield return b & 0b111;
        }
    }
}
