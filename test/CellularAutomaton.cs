using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AoC.Library.Test
{
	[TestClass]
	public class CellularAutomatonTest
	{
		enum State
		{
			Inactive,
			Active,
		}

		[TestMethod]
		public void CellularAutomaton_2020Day17Part1()
		{
			Dictionary<Vector3, State> input = new Dictionary<Vector3, State>
			{
				[new Vector3(3, 0, 0)] = State.Active,
				[new Vector3(6, 0, 0)] = State.Active,

				[new Vector3(2, 1, 0)] = State.Active,
				[new Vector3(3, 1, 0)] = State.Active,
				[new Vector3(5, 1, 0)] = State.Active,
				[new Vector3(6, 1, 0)] = State.Active,

				[new Vector3(2, 2, 0)] = State.Active,

				[new Vector3(4, 3, 0)] = State.Active,

				[new Vector3(0, 4, 0)] = State.Active,
				[new Vector3(2, 4, 0)] = State.Active,
				[new Vector3(3, 4, 0)] = State.Active,
				[new Vector3(7, 4, 0)] = State.Active,

				[new Vector3(0, 5, 0)] = State.Active,
				[new Vector3(1, 5, 0)] = State.Active,
				[new Vector3(2, 5, 0)] = State.Active,
				[new Vector3(3, 5, 0)] = State.Active,
				[new Vector3(6, 5, 0)] = State.Active,
				[new Vector3(7, 5, 0)] = State.Active,

				[new Vector3(3, 6, 0)] = State.Active,
				[new Vector3(4, 6, 0)] = State.Active,
				[new Vector3(6, 6, 0)] = State.Active,

				[new Vector3(0, 7, 0)] = State.Active,
				[new Vector3(2, 7, 0)] = State.Active,
				[new Vector3(4, 7, 0)] = State.Active,
			};

			CellularAutomaton<Vector3, State> ca = new CellularAutomaton<Vector3, State>(input)
			{
				Neighborhood = NeighborhoodKind.Moore,
				[State.Active] = static c => c[State.Active] == 2 || c[State.Active] == 3 ? State.Active : State.Inactive,
				[State.Inactive] = static c => c[State.Active] == 3 ? State.Active : State.Inactive,
			};

			for (int i = 0; i < 6; i++)
			{
				ca.Step();
			}

			Assert.AreEqual(247, ca.Count(static kvp => kvp.Value == State.Active));
		}

		[TestMethod]
		public void CellularAutomaton_2020Day17Part2()
		{
			Dictionary<Vector4, State> input = new Dictionary<Vector4, State>
			{
				[new Vector4(3, 0, 0, 0)] = State.Active,
				[new Vector4(6, 0, 0, 0)] = State.Active,

				[new Vector4(2, 1, 0, 0)] = State.Active,
				[new Vector4(3, 1, 0, 0)] = State.Active,
				[new Vector4(5, 1, 0, 0)] = State.Active,
				[new Vector4(6, 1, 0, 0)] = State.Active,

				[new Vector4(2, 2, 0, 0)] = State.Active,

				[new Vector4(4, 3, 0, 0)] = State.Active,

				[new Vector4(0, 4, 0, 0)] = State.Active,
				[new Vector4(2, 4, 0, 0)] = State.Active,
				[new Vector4(3, 4, 0, 0)] = State.Active,
				[new Vector4(7, 4, 0, 0)] = State.Active,

				[new Vector4(0, 5, 0, 0)] = State.Active,
				[new Vector4(1, 5, 0, 0)] = State.Active,
				[new Vector4(2, 5, 0, 0)] = State.Active,
				[new Vector4(3, 5, 0, 0)] = State.Active,
				[new Vector4(6, 5, 0, 0)] = State.Active,
				[new Vector4(7, 5, 0, 0)] = State.Active,

				[new Vector4(3, 6, 0, 0)] = State.Active,
				[new Vector4(4, 6, 0, 0)] = State.Active,
				[new Vector4(6, 6, 0, 0)] = State.Active,

				[new Vector4(0, 7, 0, 0)] = State.Active,
				[new Vector4(2, 7, 0, 0)] = State.Active,
				[new Vector4(4, 7, 0, 0)] = State.Active,
			};

			CellularAutomaton<Vector4, State> ca = new CellularAutomaton<Vector4, State>(input)
			{
				Neighborhood = NeighborhoodKind.Moore,
				[State.Active] = static c => c[State.Active] == 2 || c[State.Active] == 3 ? State.Active : State.Inactive,
				[State.Inactive] = static c => c[State.Active] == 3 ? State.Active : State.Inactive,
			};

			for (int i = 0; i < 6; i++)
			{
				ca.Step();
			}

			Assert.AreEqual(1392, ca.Count(static kvp => kvp.Value == State.Active));
		}
	}
}
