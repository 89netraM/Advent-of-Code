using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using static AoC.Library.Functional;

namespace AoC.Year2017
{
	[Day(7)]
	public class Day7
	{
		record Info(string name, int weight, string[] children);

		[Part(1)]
		public object Part1(string input)
		{
			Info[] infos = input.Lines()
				.Select(Parsing.PatternParser<string, int, string, Info>(
					@"(\w+) \((\d+)\)(.*)",
					static (name, weight, children) =>
						new Info(name, weight, children.Length > 0 ?
							children[4..].Split(", ") :
							new string[] { }
						)
				))
				.ToArray();

			HashSet<string> names = infos.Select(static i => i.name).ToHashSet();
			foreach (var info in infos)
			{
				foreach (var child in info.children)
				{
					names.Remove(child);
				}
			}
			return names.Single();
		}

		private class Tower
		{
			public string Name;
			public int Weight;
			public List<Tower> Children = new List<Tower>();

			public Tower(string name, int weight, List<Tower> children)
			{
				this.Name = name;
				this.Weight = weight;
				this.Children = children;
			}

			public int TotalWeight() =>
				Weight + Children.Sum(static c => c.TotalWeight());

			public int? Unbalanced()
			{
				if (Children.Count > 0)
				{
					foreach (var child in Children)
					{
						if (child.Unbalanced() is int w)
						{
							return w;
						}
					}

					var childW = Children.Select(static c => (w: c.Weight, t: c.TotalWeight())).GroupBy(static p => p.t).OrderBy(static g => g.Count()).ToArray();
					if (childW.Length == 1)
					{
						return null;
					}
					else
					{
						return childW[0].First().w + (childW[1].Key - childW[0].Key);
					}
				}
				else
				{
					return null;
				}
			}
		}


		[Part(2)]
		public object Part2(string input)
		{
			Dictionary<string, Info> infos = input.Lines()
				.Select(Parsing.PatternParser<string, int, string, Info>(
					@"(\w+) \((\d+)\)(.*)",
					static (name, weight, children) =>
						new Info(name, weight, children.Length > 0 ?
							children[4..].Split(", ") :
							new string[] { }
						)
				))
				.ToDictionary(static i => i.name, Id);

			HashSet<string> names = infos.Values.Select(static i => i.name).ToHashSet();
			foreach (var info in infos.Values)
			{
				foreach (var child in info.children)
				{
					names.Remove(child);
				}
			}

			List<Tower> MakeChildren(string[] children)
			{
				return children.Select(child => new Tower(child, infos[child].weight, MakeChildren(infos[child].children)))
					.ToList();
			}

			var root = infos[names.Single()];
			var rootTower = new Tower(root.name, root.weight, MakeChildren(root.children));

			return rootTower.Unbalanced();
		}
	}
}
