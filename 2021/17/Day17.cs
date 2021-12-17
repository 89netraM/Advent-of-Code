using System;
using AoC.Library;
using RegExtract;

namespace AoC.Year2021
{
	[Day(17)]
	public class Day17
	{
		private const long ReasonableMaxYVelocity = 1000L;

		[Part(1)]
		public object Part1(string input)
		{
			Vector2 lowerLeftTarget = input.Extract<Vector2>(@"(-?\d+)\.\.-?\d+, y=(-?\d+)");
			Vector2 upperRightTarget = input.Extract<Vector2>(@"(-?\d+), y=-?\d+\.\.(-?\d+)");

			long maxY = 0;
			for (long y = ReasonableMaxYVelocity; y >= lowerLeftTarget.Y; y--)
			{
				for (long x = 0L; x <= upperRightTarget.X; x++)
				{
					Vector2 vel = new Vector2(x, y);
					if (Follow(vel, lowerLeftTarget, upperRightTarget, out long localMaxY))
					{
						maxY = Math.Max(maxY, localMaxY);
					}
				}
			}
			return maxY;
		}

		[Part(2)]
		public object Part2(string input)
		{
			Vector2 lowerLeftTarget = input.Extract<Vector2>(@"(-?\d+)\.\.-?\d+, y=(-?\d+)");
			Vector2 upperRightTarget = input.Extract<Vector2>(@"(-?\d+), y=-?\d+\.\.(-?\d+)");

			long count = 0L;
			for (long y = ReasonableMaxYVelocity; y >= lowerLeftTarget.Y; y--)
			{
				for (long x = 0L; x <= upperRightTarget.X; x++)
				{
					Vector2 vel = new Vector2(x, y);
					if (Follow(vel, lowerLeftTarget, upperRightTarget, out long localMaxY))
					{
						count++;
					}
				}
			}
			return count;
		}

		private bool Follow(Vector2 velocity, Vector2 lowerLeftTarget, Vector2 upperRightTarget, out long height)
		{
			Vector2 position = Vector2.Zero;
			height = 0;
			for (int i = 0; true; i++)
			{
				Step(ref position, ref velocity);
				height = Math.Max(height, position.Y);
				if (position.X >= lowerLeftTarget.X && position.X <= upperRightTarget.X &&
					position.Y >= lowerLeftTarget.Y && position.Y <= upperRightTarget.Y)
				{
					return true;
				}
				else if (position.X > upperRightTarget.X || position.Y < lowerLeftTarget.Y)
				{
					return false;
				}
			}
		}

		private void Step(ref Vector2 position, ref Vector2 velocity)
		{
			position += velocity;
			if (position.X != 0)
			{
				velocity += Vector2.UnitX * -Math.Sign(velocity.X);
			}
			velocity -= Vector2.UnitY;
		}
	}
}
