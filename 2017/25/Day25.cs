using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(25)]
	public class Day25
	{
		enum State
		{
			A,
			B,
			C,
			D,
			E,
			F,
		}

		[Part(1)]
		public object Part1(string input)
		{
			Dictionary<long, long> tape = new Dictionary<long, long>();
			long cursor = 0;
			State state = State.A;

			for (long step = 0; step < 12994925L; step++)
			{
				switch (state)
				{
					case State.A:
						if (tape.GetValueOrDefault(cursor, 0) == 0)
						{
							tape[cursor] = 1;
							cursor++;
							state = State.B;
						}
						else
						{
							tape[cursor] = 0;
							cursor--;
							state = State.F;
						}
						break;
					case State.B:
						if (tape.GetValueOrDefault(cursor, 0) == 0)
						{
							tape[cursor] = 0;
							cursor++;
							state = State.C;
						}
						else
						{
							tape[cursor] = 0;
							cursor++;
							state = State.D;
						}
						break;
					case State.C:
						if (tape.GetValueOrDefault(cursor, 0) == 0)
						{
							tape[cursor] = 1;
							cursor--;
							state = State.D;
						}
						else
						{
							tape[cursor] = 1;
							cursor++;
							state = State.E;
						}
						break;
					case State.D:
						if (tape.GetValueOrDefault(cursor, 0) == 0)
						{
							tape[cursor] = 0;
							cursor--;
							state = State.E;
						}
						else
						{
							tape[cursor] = 0;
							cursor--;
							state = State.D;
						}
						break;
					case State.E:
						if (tape.GetValueOrDefault(cursor, 0) == 0)
						{
							tape[cursor] = 0;
							cursor++;
							state = State.A;
						}
						else
						{
							tape[cursor] = 1;
							cursor++;
							state = State.C;
						}
						break;
					case State.F:
						if (tape.GetValueOrDefault(cursor, 0) == 0)
						{
							tape[cursor] = 1;
							cursor--;
							state = State.A;
						}
						else
						{
							tape[cursor] = 1;
							cursor++;
							state = State.A;
						}
						break;
				}
			}

			return tape.Values.Sum();
		}
	}
}
