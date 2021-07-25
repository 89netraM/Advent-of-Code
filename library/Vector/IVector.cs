using System;

namespace AoC.Library
{
	public interface IVector<T> : IEquatable<T>, IComparable<T> where T : IVector<T>, new()
	{
		internal long Count { get; }
		internal long this[long index] { get; set; }
	}
}
