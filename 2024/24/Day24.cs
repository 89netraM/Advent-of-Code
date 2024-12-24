using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using RegExtract;

namespace AoC.Year2024;

[Day(24)]
public class Day24
{
    [Part(1)]
    public object Part1(string input)
    {
        var sections = input.Paragraphs();
        var wireValues = sections[0]
            .Lines()
            .Extract<(string, long)>(@"(\w+): (0|1)")
            .ToDictionary(p => p.Item1, p => p.Item2 is 1);
        var gates = sections[1]
            .Lines()
            .Extract<(string, string, string, string)>(@"(\w+) (AND|OR|XOR) (\w+) -> (\w+)")
            .ToArray();
        var changed = true;
        while (changed)
        {
            changed = false;
            foreach (var (input1, operation, input2, output) in gates)
            {
                if (wireValues.ContainsKey(output))
                {
                    continue;
                }

                if (!wireValues.TryGetValue(input1, out var value1))
                {
                    continue;
                }

                if (!wireValues.TryGetValue(input2, out var value2))
                {
                    continue;
                }

                wireValues[output] = operation switch
                {
                    "AND" => value1 && value2,
                    "OR" => value1 || value2,
                    "XOR" => value1 ^ value2,
                    _ => throw new InvalidOperationException($"Unknown operation: {operation}"),
                };
                changed = true;
            }
        }
        return GetOutput(wireValues);
    }

    private static long GetOutput(Dictionary<string, bool> wireValues) =>
        wireValues
            .Where(kvp => kvp.Key.StartsWith('z'))
            .Aggregate(0L, (acc, kvp) => acc | (kvp.Value ? 1L << int.Parse(kvp.Key[1..]) : 0L));

    [Part(2)]
    public object Part2(string input)
    {
        var sections = input.Paragraphs();
        var wires = sections[0]
            .Lines()
            .Extract<(string, long)>(@"(\w+): (0|1)")
            .ToDictionary(p => p.Item1, p => (IGate)new InputGate(p.Item1, p.Item2 is 1));
        var inputs = wires.Values.OfType<InputGate>().ToDictionary(g => g.Name, g => g);
        var gates = sections[1]
            .Lines()
            .Extract<(string i1, string op, string i2, string o)>(@"(\w+) (AND|OR|XOR) (\w+) -> (\w+)")
            .ToArray();

        var outputs = new Dictionary<string, OutputGate>();

        var changed = true;
        while (changed)
        {
            changed = false;
            foreach (var (input1, operation, input2, o) in gates)
            {
                var output = o;
                if (output == "mwk")
                {
                    output = "z10";
                }
                else if (output == "z10")
                {
                    output = "mwk";
                }
                if (output == "z18")
                {
                    output = "qgd";
                }
                else if (output == "qgd")
                {
                    output = "z18";
                }
                if (output == "jmh")
                {
                    output = "hsw";
                }
                else if (output == "hsw")
                {
                    output = "jmh";
                }
                if (output == "gqp")
                {
                    output = "z33";
                }
                else if (output == "z33")
                {
                    output = "gqp";
                }

                if (wires.ContainsKey(output))
                {
                    continue;
                }

                if (!wires.TryGetValue(input1, out var value1))
                {
                    continue;
                }

                if (!wires.TryGetValue(input2, out var value2))
                {
                    continue;
                }

                IGate gate = operation switch
                {
                    "AND" => new AndGate(value1, value2),
                    "OR" => new OrGate(value1, value2),
                    "XOR" => new XorGate(value1, value2),
                    _ => throw new InvalidOperationException($"Unknown operation: {operation}"),
                };
                if (output.StartsWith('z'))
                {
                    wires[output] = outputs[output] = new OutputGate(output, gate);
                }
                else
                {
                    wires[output] = gate;
                }
                changed = true;
            }
        }

        for (long i = 1; i < (1L << 44); i<<=1)
        {
            SetInput('x', i);
            for (long j = 1; j < (1L << 44); j<<=1)
            {
                SetInput('y', j);

                var expectedResult = i + j;
                var actualResult = GetOutput();

                if (expectedResult != actualResult)
                {
                    Console.WriteLine($"{i} + {j} = {expectedResult} != {actualResult}");
                    return null;
                }
            }
        }

        return null;

        void SetInput(char prefix, long value)
        {
            for (int i = 0; i < 45; i++)
            {
                inputs[$"{prefix}{i:00}"].Value = (value & (1L << i)) is not 0;
            }
        }

        long GetOutput() =>
            outputs
                .Where(kvp => kvp.Key.StartsWith('z'))
                .Aggregate(0L, (acc, kvp) => acc | (kvp.Value.Value ? 1L << int.Parse(kvp.Key[1..]) : 0L));
    }

    private interface IGate
    {
        public bool Value { get; }

        public IEnumerable<InputGate> GetAllInputs();
    }

    private class InputGate(string name, bool value) : IGate
    {
        public string Name { get; } = name;
        public bool Value { get; set; } = value;
        bool IGate.Value => Value;

        public IEnumerable<InputGate> GetAllInputs() => [this];

        public override string ToString() => Name;
    }

    private abstract class BinaryGate(IGate a, IGate b) : IGate
    {
        public IGate A { get; set; } = a;
        public IGate B { get; set; } = b;
        public abstract bool Value { get; }

        public IEnumerable<InputGate> GetAllInputs() => [.. A.GetAllInputs(), .. B.GetAllInputs()];
    }

    private class AndGate(IGate a, IGate b) : BinaryGate(a, b)
    {
        public override bool Value => A.Value && B.Value;

        public override string ToString() => $"({A}) AND ({B})";
    }

    private class OrGate(IGate a, IGate b) : BinaryGate(a, b)
    {
        public override bool Value => A.Value || B.Value;

        public override string ToString() => $"({A}) OR ({B})";
    }

    private class XorGate(IGate a, IGate b) : BinaryGate(a, b)
    {
        public override bool Value => A.Value ^ B.Value;

        public override string ToString() => $"({A}) XOR ({B})";
    }

    private class OutputGate(string name, IGate gate) : IGate
    {
        public IGate Gate { get; } = gate;
        public string Name { get; } = name;
        public bool Value => Gate.Value;

        public IEnumerable<InputGate> GetAllInputs() => Gate.GetAllInputs();

        public override string ToString() => $"{Name} = {Gate}";
    }
}
