using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;
using System.Collections.Generic;
using FsCheck;
using static AoC.Library.Functional;

namespace AoC.Library.Test
{
	[TestClass]
	public class FunctionalTest
	{
		[TestMethod]
		public void Id()
		{
			List<int> list = Enumerable.Range(0, 10).OrderByDescending(Id<int>).ToList();
			Assert.IsTrue(list.Zip(list.Skip(1), static (a, b) => (a, b)).All(static p => p.a > p.b));
		}

		[TestMethod]
		public void Not()
		{
			static bool IsLength2(string s) => s.Length == 2;
			Assert.IsTrue(new[] { "a", "b", "c" }.All(Not<string>(IsLength2)));
		}

		[TestMethod]
		public void Curry2()
		{
			static int Add((int a, int b) p) => p.a + p.b;
			Func<int, int, int> curriedAdd = Curry<int, int, int>(Add);
			Prop.ForAll<Tuple<int, int>>(p => Add((p.Item1, p.Item2)) == curriedAdd(p.Item1, p.Item2))
				.QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Uncurry2()
		{
			static int Add(int a, int b) => a + b;
			Func<(int, int), int> uncurriedAdd = Uncurry<int, int, int>(Add);
			Prop.ForAll<Tuple<int, int>>(p => Add(p.Item1, p.Item2) == uncurriedAdd((p.Item1, p.Item2)))
				.QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Curry3()
		{
			static int Sum((int a, int b, int c) p) => p.a + p.b + p.c;
			Func<int, int, int, int> curriedSum = Curry<int, int, int, int>(Sum);
			Prop.ForAll<Tuple<int, int, int>>(p => Sum((p.Item1, p.Item2, p.Item3)) == curriedSum(p.Item1, p.Item2, p.Item3))
				.QuickCheckThrowOnFailure();
		}

		[TestMethod]
		public void Uncurry3()
		{
			static int Sum(int a, int b, int c) => a + b + c;
			Func<(int, int, int), int> uncurriedSum = Uncurry<int, int, int, int>(Sum);
			Prop.ForAll<Tuple<int, int, int>>(p => Sum(p.Item1, p.Item2, p.Item3) == uncurriedSum((p.Item1, p.Item2, p.Item3)))
				.QuickCheckThrowOnFailure();
		}
	}
}
