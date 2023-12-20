using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;
using RegExtract;

namespace AoC.Year2023;

[Day(20)]
public class Day20
{
	[Part(1)]
	public object Part1(string input)
	{
		var modules = Parse(input);
		var signals = new Queue<Signal>();
		long lowCount = 0;
		long highCount = 0;
		for (int i = 0; i < 1000; i++)
		{
			signals.Enqueue(new("button", "broadcaster", false));
			while (signals.TryDequeue(out var signal))
			{
				if (signal.HighLow) { highCount++; }
				else { lowCount++; }

				foreach (var newSignal in modules[signal.Target].Process(signal))
				{
					signals.Enqueue(newSignal);
				}
			}
		}
		return lowCount * highCount;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var modules = Parse(input);
		var end = modules["rx"].Inputs.Single();
		return modules["broadcaster"].Targets
			.AsParallel()
			.Select(s => PeriodFor(modules, s, end))
			.Lcm();
	}

	private static long PeriodFor(IReadOnlyDictionary<string, Module> modules, string start, string end)
	{
		var signals = new Queue<Signal>();
		for (long i = 0; ; i++)
		{
			signals.Enqueue(new("broadcaster", start, false));
			while (signals.TryDequeue(out var signal))
			{
				foreach (var newSignal in modules[signal.Target].Process(signal))
				{
					if (newSignal.Target == end && newSignal.HighLow == true)
					{
						return i + 1; // Why +1?
					}
					signals.Enqueue(newSignal);
				}
			}
		}
	}

	private static Dictionary<string, Module> Parse(string input) =>
		input.Lines()
			.Extract<(string name, string targets)>(@"(.*?) -> (.*)")
			.Select(p => (p.name, p.targets.Split(", ")) switch
			{
				("broadcaster", var ts) => new Module(p.name, ts),
				(var n, var ts) when n[0] is '%' => new FlipFlop(n[1..], ts),
				(var n, var ts) when n[0] is '&' => new Conjunction(n[1..], ts),
				_ => throw new Exception("Unknown module type"),
			})
			.Append(new Module("rx", Array.Empty<string>()))
			.ToDictionary(m => m.Name, Id)
			.Also(modules =>
			{
				foreach (var to in modules.Values)
				{
					foreach (var from in modules.Values)
					{
						if (from.Targets.Contains(to.Name))
						{
							to.AddInput(from.Name);
						}
					}
				}
			});

	private record Module(string Name, IReadOnlyList<string> Targets)
	{
		public IReadOnlyList<string> Inputs => inputs;
		private readonly List<string> inputs = new();

		public virtual IEnumerable<Signal> Process(Signal signal) =>
			Targets.Select(t => signal with { Sender = Name, Target = t });

		public virtual void AddInput(string name) => inputs.Add(name);
	}
	private record FlipFlop(string Name, IReadOnlyList<string> Targets) : Module(Name, Targets)
	{
		private bool OnOff = false;

		public override IEnumerable<Signal> Process(Signal signal) =>
			signal.HighLow
				? Enumerable.Empty<Signal>()
				: FlipAndSend();

		private IEnumerable<Signal> FlipAndSend()
		{
			OnOff = !OnOff;
			return Targets.Select(t => new Signal(Name, t, OnOff));
		}
	}

	private record Conjunction(string Name, IReadOnlyList<string> Targets) : Module(Name, Targets)
	{
		private readonly Dictionary<string, bool> memory = new();

		public override IEnumerable<Signal> Process(Signal signal)
		{
			memory[signal.Sender] = signal.HighLow;
			var outputHighLow = !memory.Values.All(Id);
			return Targets.Select(t => new Signal(Name, t, outputHighLow));
		}

		public override void AddInput(string name)
		{
			base.AddInput(name);
			memory.Add(name, false);
		}
	}

	private record Signal(string Sender, string Target, bool HighLow);
}
