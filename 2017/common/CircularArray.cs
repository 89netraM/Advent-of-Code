using System.Collections;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	public class CircularArray : IEnumerable<int>
	{
		private readonly int[] array;

		public int Length => array.Length;

		public CircularArray(int length)
		{
			array = Enumerable.Range(0, length).ToArray();
		}

		public int this[int index]
		{
			get => array[MathM.Mod(index, array.Length)];
			set => array[MathM.Mod(index, array.Length)] = value;
		}

		public void ReverseSection(int start, int length)
		{
			int end = start + (length - 1);
			for (int i = 0; i < length / 2; i++)
			{
				(this[start + i], this[end - i]) = (this[end - i], this[start + i]);
			}
		}

		public IEnumerator<int> GetEnumerator() =>
			array.AsEnumerable().GetEnumerator();
		IEnumerator IEnumerable.GetEnumerator() =>
			GetEnumerator();
	}
}
