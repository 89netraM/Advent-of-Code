using System.Collections.Generic;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(17)]
	public class Day17
	{
		[Part(1)]
		public object Part1(string input)
		{
			int steps = int.Parse(input);

			LinkedList<int> list = new LinkedList<int>();
			var current = list.AddFirst(0);

			for (int i = 1; i <= 2017; i++)
			{
				for (int j = 0; j < steps; j++)
				{
					current = current.Next ?? list.First;
				}
				current = list.AddAfter(current, i);
			}

			return (current.Next ?? list.First).Value;
		}

		[Part(2)]
		public object Part2(string input)
		{
			int steps = int.Parse(input);
			int current = 0;
			int afterZero = 0;

			for (int i = 1; i <= 50000000; i++)
			{
				current = MathM.Mod(current + steps, i) + 1;
				if (current == 1)
				{
					afterZero = i;
				}
			}

			return afterZero;
		}
	}
}
