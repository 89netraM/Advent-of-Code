using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	public static class KnotHash
	{
		public static byte[] Calculate(string input)
		{
			int[] lengths = input.Select(static c => (int)c).Concat(new[] { 17, 31, 73, 47, 23 }).ToArray();
			CircularArray array = new CircularArray(256);

			var currPos = 0;
			int skipSize = 0;
			for (int i = 0; i < 64; i++)
			{
				foreach (int length in lengths)
				{
					array.ReverseSection(currPos, length);
					currPos = MathM.Mod(currPos + length + skipSize, array.Length);
					skipSize++;
				}
			}

			return array.Select(static (x, i) => (x, i))
				.GroupBy(static p => p.i / 16)
				.Select(static g => g.Aggregate<(int x, int i), byte>(0, (a, b) => (byte)(a ^ b.x)))
				.ToArray();
		}
	}
}
