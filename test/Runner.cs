using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using System;
using TextCopy;

namespace AoC.Library.Test
{
	[TestClass]
	public class RunnerTest
	{
		[TestMethod]
		public void TestDay1Part1()
		{
			TextWriter standardOut = Console.Out;
			try
			{
				StringWriter sw = new StringWriter();
				Console.SetOut(sw);

				Runner.Run(new[] { "1", "1" });
				Assert.AreEqual($"test input{Environment.NewLine}", sw.ToString());
				Assert.AreEqual("test input", ClipboardService.GetText());
			}
			finally
			{
				Console.SetOut(standardOut);
			}
		}
	}

	[Day(1)]
	public class Day1Test
	{
		[Part(1)]
		public string Part1(string input) =>
			input;
	}
}
