using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2021
{
	[Day(10)]
	public class Day10
	{
		[Part(1)]
		public object Part1(string input)
		{
			long score = 0;
			foreach (var line in input.Lines())
			{
				Stack<char> stack = new Stack<char>();
				foreach (var c in line)
				{
					if (c is '(' or '[' or '{' or '<')
					{
						stack.Push(c);
					}
					else
					{
						score += (stack.Pop(), c) switch
						{
							('(', ')') => 0,
							('[', ']') => 0,
							('{', '}') => 0,
							('<', '>') => 0,
							(_, ')') => 3,
							(_, ']') => 57,
							(_, '}') => 1197,
							(_, '>') => 25137,
							_ => throw new Exception(),
						};
					}
				}
			}
			return score;
		}

		[Part(2)]
		public object Part2(string input)
		{
			List<long> scores = new List<long>();
			foreach (var line in input.Lines())
			{
				long localScore = 0;
				Stack<char> stack = new Stack<char>();
				foreach (var c in line)
				{
					if (c is '(' or '[' or '{' or '<')
					{
						stack.Push(c);
					}
					else
					{
						localScore += (stack.Pop(), c) switch
						{
							('(', ')') => 0,
							('[', ']') => 0,
							('{', '}') => 0,
							('<', '>') => 0,
							(_, ')') => 3,
							(_, ']') => 57,
							(_, '}') => 1197,
							(_, '>') => 25137,
							_ => throw new Exception(),
						};
					}
				}
				if (localScore == 0)
				{
					localScore = 0;
					foreach (var c in stack)
					{
						localScore *= 5;
						localScore += c switch
						{
							'(' => 1,
							'[' => 2,
							'{' => 3,
							'<' => 4,
							_ => throw new Exception(),
						};
					}
					scores.Add(localScore);
				}
			}
			return scores.OrderBy(Id).Skip(scores.Count / 2).First();
		}
	}
}
