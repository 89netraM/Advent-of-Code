using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2022;

[Day(7)]
public class Day7
{
	[Part(1)]
	public object Part1(string input)
	{
		var root = ParseFilesystem(input);

		long sum = 0;
		void DirVisitor(Dir dir)
		{
			if (dir.Size <= 100000)
			{
				sum += dir.Size;
			}
			foreach (var child in dir.Items.Values)
			{
				if (child is Dir childDir)
				{
					DirVisitor(childDir);
				}
			}
		}
		root.Visit(DirVisitor);
		return sum;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var root = ParseFilesystem(input);

		long availableSpace = 70000000 - root.Size;
		long missingSpace = 30000000 - availableSpace;

		long smallest = Int64.MaxValue;
		void DirVisitor(Dir dir)
		{
			if (dir.Size >= missingSpace && dir.Size < smallest)
			{
				smallest = dir.Size;
			}
			foreach (var child in dir.Items.Values)
			{
				if (child is Dir childDir)
				{
					DirVisitor(childDir);
				}
			}
		}
		root.Visit(DirVisitor);
		return smallest;
	}

	private Dir ParseFilesystem(string input)
	{
		var root = new Dir("/", new Dictionary<string, IItem>());

		var path = new List<(string p, Dir d)>();

		var commands = input.Lines().ToArray();
		for (int i = 0; i < commands.Length; i++)
		{
			var command = commands[i];
			if (command == "$ cd /")
			{
				path.Clear();
				path.Add(("", root));
			}
			else if (command == "$ cd ..")
			{
				path.RemoveAt(path.Count - 1);
			}
			else if (command.StartsWith("$ cd "))
			{
				var name = command[5..];
				if (path[^1].d.Items.TryGetValue(name, out IItem item) && item is Dir nextDir)
				{
					path.Add((name, nextDir));
				}
				else
				{
					nextDir = new Dir(name, new Dictionary<string, IItem>());
					path[^1].d.Items.Add(name, nextDir);
					path.Add((name, nextDir));
				}
			}
			else if (command == "$ ls")
			{
				for (i += 1; i < commands.Length && !commands[i].StartsWith("$"); i++)
				{
					var item = commands[i];
					if (!item.StartsWith("dir"))
					{
						var sections = item.Split(" ");
						var size = Int64.Parse(sections[0]);
						var name = sections[1];
						path[^1].d.Items.Add(name, new File(name, size));
					}
				}
				if (i < commands.Length && commands[i].StartsWith("$"))
				{
					i--;
				}
			}
		}

		return root;
	}

	interface IItem
	{
		string Name { get; }
		long Size { get; }
	}
	record Dir(string Name, Dictionary<string, IItem> Items) : IItem
	{
		public long Size => Items.Values.Sum(i => i.Size);

		public void Visit(Action<Dir> visitor) =>
			visitor(this);
	}
	record File(string Name, long Size) : IItem;
}
