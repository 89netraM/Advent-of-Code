using AoC.Library;

namespace AoC.Year2017
{
	[Day(9)]
	public class Day9
	{
		[Part(1)]
		public object Part1(string input)
		{
			int score = 0;
			int level = 1;
			bool isGarbage = false;
			for (int i = 0; i < input.Length; i++)
			{
				char c = input[i];
				if (c == '!')
				{
					i++;
				}
				else
				{
					if (!isGarbage)
					{
						if (c == '{')
						{
							score += level;
							level++;
						}
						else if (c == '}')
						{
							level--;
						}
						else if (c == '<')
						{
							isGarbage = true;
						}
					}
					else
					{
						if (c == '>')
						{
							isGarbage = false;
						}
					}
				}
			}
			return score;
		}

		[Part(2)]
		public object Part2(string input)
		{
			int garbage = 0;
			int level = 1;
			bool isGarbage = false;
			for (int i = 0; i < input.Length; i++)
			{
				char c = input[i];
				if (c == '!')
				{
					i++;
				}
				else
				{
					if (!isGarbage)
					{
						if (c == '{')
						{
							level++;
						}
						else if (c == '}')
						{
							level--;
						}
						else if (c == '<')
						{
							isGarbage = true;
						}
					}
					else
					{
						if (c == '>')
						{
							isGarbage = false;
						}
						else
						{
							garbage++;
						}
					}
				}
			}
			return garbage;
		}
	}
}
