using System.Linq;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(18)]
	public class Day18
	{
		[Part(1)]
		public object Part1(string input)
		{
			var ca = new CellularAutomaton<Vector2, State>(input.ToMap<State>(c => c == '#' ? State.On : State.Off))
			{
				Neighborhood = NeighborhoodKind.Moore,
				ConfinementBounds = (new(0, 0), new(99, 99)),
				[State.Off] = c => c[State.On] == 3 ? State.On : State.Off,
				[State.On] = c => c[State.On] == 2 || c[State.On] == 3 ? State.On : State.Off,
			};

			for (int i = 0; i < 100; i++)
			{
				ca.Step();
			}

			return ca.Count(kvp => kvp.Value == State.On);
		}

		[Part(2)]
		public object Part2(string input)
		{
			var ca = new CellularAutomaton<Vector2, State>(input.ToMap<State>(c => c == '#' ? State.On : State.Off))
			{
				Neighborhood = NeighborhoodKind.Moore,
				ConfinementBounds = (new(0, 0), new(99, 99)),
				[State.Off] = c => c[State.On] == 3 ? State.On : State.Off,
				[State.On] = c => c[State.On] == 2 || c[State.On] == 3 ? State.On : State.Off,
			};

			for (int i = 0; i < 100; i++)
			{
				ca.Step();
				ca[new Vector2(0, 0)] = State.On;
				ca[new Vector2(0, 99)] = State.On;
				ca[new Vector2(99, 0)] = State.On;
				ca[new Vector2(99, 99)] = State.On;
			}

			return ca.Count(kvp => kvp.Value == State.On);
		}

		private enum State
		{
			Off,
			On,
		}
	}
}
