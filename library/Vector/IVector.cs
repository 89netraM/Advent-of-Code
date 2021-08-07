using System;
using System.Collections.Generic;

namespace AoC.Library
{
	public interface IVector<T> : IEquatable<T>, IComparable<T>, IEnumerable<long> where T : IVector<T>
	{
		internal long Count { get; }
		internal long this[long index] { get; }
	}
}
