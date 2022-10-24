using System;

namespace AoC.Library;

public class Instruction
{
	private readonly string[] parts;

	public string Op => parts[0];

	public string this[int index] => parts[index + 1];

	public Instruction(string line) =>
		parts = line.Words();

	public long Number(int index) =>
		TryNumber(index, out long v)
			? v
			: throw new InvalidOperationException($"Argument {index} of \"{String.Join(" ", parts)}\" is not a valid integer number");

	public bool TryNumber(int index, out long value) =>
		Int64.TryParse(this[index], out value);
}
