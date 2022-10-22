using AoC.Library;
using System.Text;

namespace AoC.Year2016;

[Day(9)]
public class Day9
{
	[Part(1)]
	public object Part1(string input)
	{
		var sb = new StringBuilder();

		for (int i = 0; i < input.Length; i++)
		{
			if (input[i] == '(')
			{
				i++;
				var (length, reps) = ParseMarker(input, ref i);
				for (int r = 0; r < reps; r++)
				{
					sb.Append(input.Substring(i, length));
				}
				i += length - 1;
			}
			else
			{
				sb.Append(input[i]);
			}
		}

		return sb.Length;
	}

	[Part(2)]
	public long Part2(string input)
	{
		var length = 0L;

		for (int i = 0; i < input.Length; i++)
		{
			if (input[i] == '(')
			{
				i++;
				var (len, reps) = ParseMarker(input, ref i);
				length += Part2(input.Substring(i, len)) * reps;
				i += len - 1;
			}
			else
			{
				length++;
			}
		}

		return length;
	}

	private (int, int) ParseMarker(string input, ref int i)
	{
		var length = ParseNumber(input, ref i, 'x');
		i++;
		var reps = ParseNumber(input, ref i, ')');
		i++;
		return (length, reps);
	}

	private int ParseNumber(string input, ref int i, char end)
	{
		int num = 0;
		for (; i < input.Length && input[i] != end; i++)
		{
			num = num * 10 + (int)(input[i] - '0');
		}
		return num;
	}
}
