using AoC.Library;

namespace AoC.Year2016;

[Day(18)]
public class Day18
{
	[Part(1)]
	public object Part1(string input) =>
		CountSafePlates(input, 40);

	[Part(2)]
	public object Part2(string input) =>
		CountSafePlates(input, 400000);

	private long CountSafePlates(string input, int rows)
	{
		var ca = new CellularAutomaton<Vector2, Plate>(input.ToMap(c => c == '^' ? Plate.Trap : Plate.Safe))
		{
			[Plate.Safe] = ns => ns[Plate.Trap] == 1 ? Plate.Trap : Plate.Safe,
			[Plate.Trap] = ns => ns[Plate.Trap] == 1 ? Plate.Trap : Plate.Safe,
			ConfinementBounds = (Vector2.Zero, new Vector2(input.Length - 1, 0)),
		};
		long count = ca.CountInBounds(s => s == Plate.Safe);
		for (int i = 0; i < rows - 1; i++)
		{
			ca.Step();
			count += ca.CountInBounds(s => s == Plate.Safe);
		}
		return count;
	}
	private enum Plate
	{
		Safe,
		Trap,
	}
}
