using System;

namespace AoC.Library
{
	[AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
	public class DayAttribute : Attribute
	{
		public int Day { get; }

		public DayAttribute(int day) =>
			(Day) = (day);
	}
}
