using System;
using System.Linq;
using AoC.Library;
using OneOf;

namespace AoC.Year2021
{
	[Day(18)]
	public class Day18
	{
		[Part(1)]
		public object Part1(string input)
		{
			var lines = input.Lines().ToArray();
			var math = SnailFishMath.Parse(lines[0]);
			foreach (var line in lines[1..])
			{
				var next = SnailFishMath.Parse(line);
				math += next;
				math = math.Reduce();
			}
			return math.Magnitude();
		}

		[Part(2)]
		public object Part2(string input)
		{
			var maths = input.Lines().Select(SnailFishMath.Parse).ToArray();
			long max = 0;
			for (int i = 0; i < maths.Length; i++)
			{
				for (int j = i; j < maths.Length; j++)
				{
					max = Math.Max(max, (maths[i] + maths[j]).Reduce().Magnitude());
					max = Math.Max(max, (maths[j] + maths[i]).Reduce().Magnitude());
				}
			}
			return max;
		}
	}

	public class SnailFishMath : ICloneable, IEquatable<SnailFishMath>
	{
		public static SnailFishMath Parse(string input) =>
			InternalParse(input, 0).math;
		private static (SnailFishMath math, int length) InternalParse(string input, int offset)
		{
			int i = offset + 1;

			OneOf<long, SnailFishMath> left;
			if (input[i] == '[')
			{
				var (math, length) = InternalParse(input, i);
				left = math;
				i += length;
			}
			else
			{
				var leftString = input.Substring(i, input.IndexOf(',', i) - i);
				left = long.Parse(leftString);
				i += leftString.Length;
			}
			i++;

			OneOf<long, SnailFishMath> right;
			if (input[i] == '[')
			{
				var (math, length) = InternalParse(input, i);
				right = math;
				i += length;
			}
			else
			{
				var rightString = input.Substring(i, input.IndexOf(']', i) - i);
				right = long.Parse(rightString);
				i += rightString.Length;
			}
			i++;

			return (new SnailFishMath(left, right), i - offset);
		}

		public OneOf<long, SnailFishMath> Left { get; }
		public OneOf<long, SnailFishMath> Right { get; }

		public SnailFishMath(OneOf<long, SnailFishMath> left, OneOf<long, SnailFishMath> right) =>
			(Left, Right) = (left, right);

		public SnailFishMath Reduce()
		{
			SnailFishMath math = null;
			SnailFishMath next = this;
			while (next is not null)
			{
				math = next;
				next = math.ReduceOnce();
			}
			return math;
		}
		public SnailFishMath ReduceOnce() =>
			ReduceOnceExplode(0)?.math.AsT1 ?? ReduceOnceSplit(0)?.AsT1;
		private OneOf<long, SnailFishMath>? ReduceOnceSplit(int depth)
		{
			if (Left.IsT0 && Left.AsT0 >= 10L)
			{
				var half = Left.AsT0 / 2.0d;
				return new SnailFishMath(new SnailFishMath((long)Math.Floor(half), (long)Math.Ceiling(half)), Right);
			}
			else if (Left.Match(static l => null, m => m.ReduceOnceSplit(depth + 1)) is OneOf<long, SnailFishMath> newMathL)
			{
				return new SnailFishMath(newMathL, Right);
			}
			else if (Right.Match(static l => null, m => m.ReduceOnceSplit(depth + 1)) is OneOf<long, SnailFishMath> newMathR)
			{
				return new SnailFishMath(Left, newMathR);
			}
			else if (Right.IsT0 && Right.AsT0 >= 10L)
			{
				var half = Right.AsT0 / 2.0d;
				return new SnailFishMath(Left, new SnailFishMath((long)Math.Floor(half), (long)Math.Ceiling(half)));
			}
			else
			{
				return null;
			}
		}
		private (long, OneOf<long, SnailFishMath> math, long)? ReduceOnceExplode(int depth)
		{
			if (depth >= 4)
			{
				if (!Left.IsT0)
				{
					throw new Exception($"Left is not a number at depth {depth}");
				}
				if (!Right.IsT0)
				{
					throw new Exception($"Right is not a number at depth {depth}");
				}

				return (Left.AsT0, 0L, Right.AsT0);
			}
			else if (Left.Match(l => null, m => m.ReduceOnceExplode(depth + 1)) is var (leftL, newMathL, rightL))
			{
				return (leftL, new SnailFishMath(newMathL, rightL == 0 ? Right : AddLeft(Right, rightL)), 0L);
			}
			else if (Right.Match(l => null, m => m.ReduceOnceExplode(depth + 1)) is var (leftR, newMathR, rightR))
			{
				return (0L, new SnailFishMath(leftR == 0 ? Left : AddRight(Left, leftR), newMathR), rightR);
			}
			else
			{
				return null;
			}
		}
		private static OneOf<long, SnailFishMath> AddRight(OneOf<long, SnailFishMath> thing, long value) =>
			thing.Match<OneOf<long, SnailFishMath>>(l => l + value, m => new SnailFishMath(m.Left, AddRight(m.Right, value)));
		private static OneOf<long, SnailFishMath> AddLeft(OneOf<long, SnailFishMath> thing, long value) =>
			thing.Match<OneOf<long, SnailFishMath>>(l => l + value, m => new SnailFishMath(AddLeft(m.Left, value), m.Right));

		public long Magnitude() =>
			Left.Match(l => l, m => m.Magnitude()) * 3 + 2 * Right.Match(l => l, m => m.Magnitude());

		public override string ToString() =>
			$"[{Left.Value.ToString()},{Right.Value.ToString()}]";

		public SnailFishMath Clone() =>
			new SnailFishMath(Left.Match<OneOf<long, SnailFishMath>>(l => l, m => m.Clone()), Right.Match<OneOf<long, SnailFishMath>>(l => l, m => m.Clone()));
		object ICloneable.Clone() =>
			Clone();

		public bool Equals(SnailFishMath other) =>
			Left.Value.Equals(other.Left.Value) && Right.Value.Equals(other.Right.Value);

		public override bool Equals(object obj) =>
			obj is SnailFishMath other && Equals(other);

		public override int GetHashCode() =>
			HashCode.Combine(Left.Value, Right.Value);

		public static bool operator ==(SnailFishMath left, SnailFishMath right) =>
			left.Equals(right);

		public static bool operator !=(SnailFishMath left, SnailFishMath right) =>
			!left.Equals(right);

		public static SnailFishMath operator +(SnailFishMath left, SnailFishMath right) =>
			new SnailFishMath(left, right);
	}
}
