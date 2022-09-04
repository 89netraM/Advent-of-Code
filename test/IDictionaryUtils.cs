using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class IDictionaryUtilsTest
	{
		[TestMethod]
		public void AddOrUpdate_ShouldNotRunUpdaterWhenAdding()
		{
			var dict = new Dictionary<int, int>();
			dict.AddOrUpdate(0, () => 1, oldValue => { Assert.Fail(); return oldValue; });
		}

		[TestMethod]
		public void AddOrUpdate_ShouldNotRunCreatorWhenUpdating()
		{
			var dict = new Dictionary<int, int>
			{
				[0] = 1,
			};
			dict.AddOrUpdate(0, () => { Assert.Fail(); return 1; }, oldValue => oldValue + 1);
		}

		[TestMethod]
		public void AddOrUpdate_ShouldUpdateValue()
		{
			var dict = new Dictionary<int, int>
			{
				[0] = 1,
			};
			dict.AddOrUpdate(0, () => 1, oldValue => oldValue + 1);
			Assert.AreEqual(2, dict[0]);
		}

		[TestMethod]
		public void AddOrUpdate_ShouldCreateValue()
		{
			var dict = new Dictionary<int, int>();
			dict.AddOrUpdate(0, () => 1, oldValue => oldValue + 1);
			Assert.AreEqual(1, dict[0]);
		}

		[TestMethod]
		public void AddOrModify_ShouldNotRunModifyWhenAdding()
		{
			var dict = new Dictionary<int, IList<int>>();
			dict.AddOrModify(0, () => new List<int> { 1 }, list => Assert.Fail());
		}

		[TestMethod]
		public void AddOrModify_ShouldNotRunCreatorWhenModifying()
		{
			var dict = new Dictionary<int, IList<int>>
			{
				[0] = new List<int> { 1 },
			};
			dict.AddOrModify(0, () => { Assert.Fail(); return null!; }, list => list.Add(2));
		}

		[TestMethod]
		public void AddOrUpdate_ShouldCreateObject()
		{
			var dict = new Dictionary<int, IList<int>>();
			dict.AddOrModify(0, () => new List<int> { 1 }, list => list.Add(2));
			Assert.IsTrue(dict[0].SequenceEqual(new[] { 1 }));
		}

		[TestMethod]
		public void AddOrUpdate_ShouldModifyObject()
		{
			var dict = new Dictionary<int, IList<int>>
			{
				[0] = new List<int> { 1 },
			};
			dict.AddOrModify(0, () => new List<int> { 1 }, list => list.Add(2));
			Assert.IsTrue(dict[0].SequenceEqual(new[] { 1, 2 }));
		}
	}
}
