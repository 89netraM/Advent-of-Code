using System;

namespace AoC.Library
{
	[AttributeUsage(AttributeTargets.Method, AllowMultiple = false)]
	public class PartAttribute : Attribute
	{
		public int Part { get; }

		public PartAttribute(int part) =>
			(Part) = (part);
	}
}
