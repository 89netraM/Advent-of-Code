using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2016;

[Day(18)]
public class Day18
{
	private enum Plate
	{
		Safe,
		Trap,
	}

	[Part(1)]
	public object Part1(string input)
	{
		var ca = new CellularAutomaton<Vector2, Plate>(input.ToMap(c => c == '^' ? Plate.Trap : Plate.Safe))
		{
			[Plate.Safe] = ns => ns[Plate.Trap] == 1 ? Plate.Trap : Plate.Safe,
			[Plate.Trap] = ns => ns[Plate.Trap] == 1 ? Plate.Trap : Plate.Safe,
			ConfinementBounds = (Vector2.Zero, new Vector2(input.Length - 1, 0)),
		};
		long count = ca.Count(kvp => kvp.Value == Plate.Safe);
		for (int i = 0; i < 39; i++)
		{
			ca.Step();
			count += input.Length - ca.Count();
		}
		return count;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var ca = new CellularAutomaton<Vector2, Plate>(input.ToMap(c => c == '^' ? Plate.Trap : Plate.Safe))
		{
			[Plate.Safe] = ns => ns[Plate.Trap] == 1 ? Plate.Trap : Plate.Safe,
			[Plate.Trap] = ns => ns[Plate.Trap] == 1 ? Plate.Trap : Plate.Safe,
			ConfinementBounds = (Vector2.Zero, new Vector2(input.Length - 1, 0)),
		};
		long count = ca.Count(kvp => kvp.Value == Plate.Safe);
		for (int i = 0; i < 399999; i++)
		{
			ca.Step();
			count += input.Length - ca.Count();
		}
		return count;
	}
}
