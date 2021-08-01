using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public partial class ParsingTest
	{
		[TestMethod]
		public void PatternParser1_MatchAndNotMatch()
		{
			Func<string, double?> heightParser = Parsing.PatternParser<double?>(@"height: (\d+(?:\.\d+)?)");
			Assert.AreEqual(1.93, heightParser("height: 1.93m"));
			Assert.IsNull(heightParser("age: 22 years"));
		}

		[TestMethod]
		public void PatternParser1_Select()
		{
			IEnumerable<string> lines = new string[]
			{
				"height: 1.93m",
				"height: 2.18m",
				"height: 2.08m",
			};
			Assert.IsTrue(
				lines.Select(Parsing.PatternParser<double>(@"height: (\d+(?:\.\d+)?)"))
					.SequenceEqual(new double[]
					{
						1.93d,
						2.18d,
						2.08d,
					})
			);
		}

		[TestMethod]
		public void PatternParser2_MatchAndNotMatch()
		{
			Func<string, (string, int)?> nameAgeParser = Parsing.PatternParser<string, int>(@"name: ([^,]+), age: (\d+)");
			Assert.AreEqual(("Mårten Åsberg", 22), nameAgeParser("name: Mårten Åsberg, age: 22"));
			Assert.IsNull(nameAgeParser("height: 1.93m"));
		}

		[TestMethod]
		[ExpectedException(typeof(ArgumentException))]
		public void PatternParser2_GroupMismatch()
		{
			_ = Parsing.PatternParser<string, int>(@"name: ([^,]+)");
		}

		[TestMethod]
		public void PatternParser3_MatchAndNotMatch()
		{
			Func<string, (string, int, double)?> nameAgeHeightParser = Parsing.PatternParser<string, int, double>(@"name: ([^,]+), age: (\d+), height: (\d+(?:\.\d+))");
			Assert.AreEqual(("Mårten Åsberg", 22, 1.93), nameAgeHeightParser("name: Mårten Åsberg, age: 22, height: 1.93"));
			Assert.IsNull(nameAgeHeightParser("name: Mårten Åsberg, height: 1.93, age: 22"));
		}

		[TestMethod]
		[ExpectedException(typeof(FormatException))]
		public void PatternParser3_GroupMismatch()
		{
			Func<string, (string, int, double)?> nameAgeHeightParser = Parsing.PatternParser<string, int, double>(@"name: ([^,]+), age: ([^,]+), height: ([^,]+)");
			_ = nameAgeHeightParser("name: Mårten Åsberg, age: twenty-two, height: 1.93");
		}

		[TestMethod]
		public void PatternParser4_MatchAndNotMatch()
		{
			Func<string, (string, int, double, bool)?> nameAgeHeightIsAliveParser = Parsing.PatternParser<string, int, double, bool>(@"name: ([^,]+), age: (\d+), height: (\d+(?:\.\d+)), is alive: (true|false)");
			Assert.AreEqual(("Mårten Åsberg", 22, 1.93, true), nameAgeHeightIsAliveParser("name: Mårten Åsberg, age: 22, height: 1.93, is alive: true"));
			Assert.IsNull(nameAgeHeightIsAliveParser("name: Mårten Åsberg, height: 1.93, age: 22, is alive: false"));
		}

		record Person(string name, int age, double height, bool isAlive);
		static Person MakePerson(string name, int age, double height, bool isAlive) =>
			new Person(name, age, height, isAlive);
		[TestMethod]
		public void PatternParser4_Combiner()
		{
			Func<string, Person?> personParser = Parsing.PatternParser<string, int, double, bool, Person>(@"name: ([^,]+), age: (\d+), height: (\d+(?:\.\d+)), is alive: (true|false)", MakePerson);
			Assert.AreEqual(new Person("Mårten Åsberg", 22, 1.93, true), personParser("name: Mårten Åsberg, age: 22, height: 1.93, is alive: true"));
			Assert.IsNull(personParser("name: Mårten Åsberg, height: 1.93, age: 22, is alive: false"));
		}
	}
}
