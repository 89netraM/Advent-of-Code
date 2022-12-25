using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

#pragma warning disable CS8509

[Day(25)]
public class Day25
{
	[Part(1)]
	public object Part1(string input) =>
		input.Lines().Aggregate(Add);

	private string Add(string a, string b)
	{
		int length = Math.Max(a.Length, b.Length);
		var aChars = a.Reverse().Concat(Enumerable.Repeat('0', int.MaxValue)).Take(length);
		var bChars = b.Reverse().Concat(Enumerable.Repeat('0', int.MaxValue)).Take(length);

		var result = new Stack<char>();
		char remainder = '0';
		foreach (var (aC, bC) in aChars.Zip(bChars))
		{
			(remainder, var res) = Add(aC, bC, remainder);
			result.Push(res);
		}
		result.Push(remainder);
		while (result.Peek() == '0') result.Pop();

		return String.Concat(result);
	}

	private (char, char) Add(char a, char b, char remainder)
	{
		var (rem1, res) = Add(a, b);
		(var rem2, res) = Add(res, remainder);
		var (_, rem) = Add(rem1, rem2);
		return (rem, res);
	}

	private (char, char) Add(char a, char b) =>
		(a, b) switch
		{
			('=', '=') => ('-', '1'),
			('=', '-') => ('-', '2'),
			('=', '0') => ('0', '='),
			('=', '1') => ('0', '-'),
			('=', '2') => ('0', '0'),
			('-', '=') => ('-', '2'),
			('-', '-') => ('0', '='),
			('-', '0') => ('0', '-'),
			('-', '1') => ('0', '0'),
			('-', '2') => ('0', '1'),
			('0', '=') => ('0', '='),
			('0', '-') => ('0', '-'),
			('0', '0') => ('0', '0'),
			('0', '1') => ('0', '1'),
			('0', '2') => ('0', '2'),
			('1', '=') => ('0', '-'),
			('1', '-') => ('0', '0'),
			('1', '0') => ('0', '1'),
			('1', '1') => ('0', '2'),
			('1', '2') => ('1', '='),
			('2', '=') => ('0', '0'),
			('2', '-') => ('0', '1'),
			('2', '0') => ('0', '2'),
			('2', '1') => ('1', '='),
			('2', '2') => ('1', '-'),
		};
}
