using System;
using System.Collections.Generic;
using AoC.Library;
using RegExtract;

#nullable enable

namespace AoC.Year2015
{
	[Day(7)]
	public class Day7
	{
		private interface IEvaluative
		{
			ushort? Evaluate(IReadOnlyDictionary<string, ushort> values);
		}

		private record Wire(string name) : IEvaluative
		{
			public ushort? Evaluate(IReadOnlyDictionary<string, ushort> values) =>
				values.TryGetValue(name, out ushort value) ? value : null;
		}

		private record Constant(ushort value) : IEvaluative
		{
			public ushort? Evaluate(IReadOnlyDictionary<string, ushort> values) =>
				value;
		}

		private abstract record Dual(IEvaluative a, IEvaluative b) : IEvaluative
		{
			public ushort? Evaluate(IReadOnlyDictionary<string, ushort> values)
			{
				var aValue = a.Evaluate(values);
				var bValue = b.Evaluate(values);
				if (aValue is ushort aShort && bValue is ushort bShort)
				{
					return Calculate(aShort, bShort);
				}
				else
				{
					return null;
				}
			}

			protected abstract ushort Calculate(ushort a, ushort b);
		}

		private record And(IEvaluative a, IEvaluative b) : Dual(a, b)
		{
			protected override ushort Calculate(ushort a, ushort b)
			{
				unchecked
				{
					return (ushort)(a & b);
				}
			}
		}

		private record Or(IEvaluative a, IEvaluative b) : Dual(a, b)
		{
			protected override ushort Calculate(ushort a, ushort b)
			{
				unchecked
				{
					return (ushort)(a | b);
				}
			}
		}

		private record LShift(IEvaluative a, IEvaluative b) : Dual(a, b)
		{
			protected override ushort Calculate(ushort a, ushort b)
			{
				unchecked
				{
					return (ushort)(a << b);
				}
			}
		}

		private record RShift(IEvaluative a, IEvaluative b) : Dual(a, b)
		{
			protected override ushort Calculate(ushort a, ushort b)
			{
				unchecked
				{
					return (ushort)(a >> b);
				}
			}
		}

		private record Not(IEvaluative input) : IEvaluative
		{
			public ushort? Evaluate(IReadOnlyDictionary<string, ushort> values)
			{
				if (input.Evaluate(values) is ushort inputShort)
				{
					unchecked
					{
						return (ushort)(~inputShort);
					}
				}
				else
				{
					return null;
				}
			}
		}

		[Part(1)]
		public ushort Part1(string input)
		{
			var wires = Parse(input);
			var values = new Dictionary<string, ushort>();
			while (!values.ContainsKey("a"))
			{
				foreach (var (wire, name) in wires)
				{
					if (!values.ContainsKey(name) && wire.Evaluate(values) is ushort value)
					{
						values[name] = value;
					}
				}
			}
			return values["a"];
		}

		[Part(2)]
		public ushort Part2(string input)
		{
			var wires = Parse(input);
			var values = new Dictionary<string, ushort>
			{
				["b"] = Part1(input),
			};
			while (!values.ContainsKey("a"))
			{
				foreach (var (wire, name) in wires)
				{
					if (!values.ContainsKey(name) && wire.Evaluate(values) is ushort value)
					{
						values[name] = value;
					}
				}
			}
			return values["a"];
		}

		private IReadOnlyCollection<(IEvaluative, string)> Parse(string input)
		{
			var wires = new List<(IEvaluative, string)>();
			foreach (var line in input.Lines())
			{
				var (wire, name) = line.Extract<(string, string)>(@"^(.*?) -> (\w*)$");
				wires.Add((ParseWire(wire), name));
			}
			return wires;
		}

		private IEvaluative ParseWire(string wire)
		{
			if (UInt16.TryParse(wire, out ushort constant))
			{
				return new Constant(constant);
			}
			else if (wire.Extract<(string, string)?>(@"(?:(.*) AND (.*))?") is (string aA, string bA))
			{
				return new And(WireOrConstant(aA), WireOrConstant(bA));
			}
			else if (wire.Extract<(string, string)?>(@"(?:(.*) OR (.*))?") is (string aO, string bO))
			{
				return new Or(WireOrConstant(aO), WireOrConstant(bO));
			}
			else if (wire.Extract<(string, string)?>(@"(?:(.*) LSHIFT (.*))?") is (string aL, string bL))
			{
				return new LShift(WireOrConstant(aL), WireOrConstant(bL));
			}
			else if (wire.Extract<(string, string)?>(@"(?:(.*) RSHIFT (.*))?") is (string aR, string bR))
			{
				return new RShift(WireOrConstant(aR), WireOrConstant(bR));
			}
			else if (wire.Extract<string?>(@"(?:NOT (.*))?") is string inputNot)
			{
				return new Not(WireOrConstant(inputNot));
			}
			else
			{
				return new Wire(wire);
			}
		}

		private IEvaluative WireOrConstant(string input) =>
			UInt16.TryParse(input, out ushort constant) ?
				new Constant(constant) :
				new Wire(input);
	}
}
