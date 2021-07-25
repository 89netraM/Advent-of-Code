using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AoC.Library
{
	public static class Vector
	{
		public static T Zero<T>() where T : IVector<T>, new() =>
			new T();

		public static IEnumerable<T> DirectionsMoore<T>() where T : IVector<T>, new() =>
			Zero<T>().NeighborsMoore();
		public static IEnumerable<T> DirectionsVonNeumann<T>() where T : IVector<T>, new() =>
			Zero<T>().NeighborsVonNeumann();

		public static IEnumerable<T> NeighborsMoore<T>(this T v, long range = 1) where T : IVector<T>, new() =>
			v.NeighborsMooreRecursive(range, new List<long>());
		private static IEnumerable<T> NeighborsMooreRecursive<T>(this T v, long range, IList<long> taken) where T : IVector<T>, new()
		{
			if (taken.Count == v.Count)
			{
				if (taken.Any(static x => x != 0))
				{
					T t = new T();
					for (int i = 0; i < v.Count; i++)
					{
						t[i] = v[i] + taken[(int)v.Count - i - 1];
					}
					yield return t;
				}
			}
			else
			{
				for (long i = -range; i <= range; i++)
				{
					taken.Add(i);
					foreach (T t in v.NeighborsMooreRecursive(range, taken))
					{
						yield return t;
					}
					taken.RemoveAt(taken.Count - 1);
				}
			}
		}

		public static IEnumerable<T> NeighborsVonNeumann<T>(this T v, long range = 1) where T : IVector<T>, new() =>
			v.NeighborsVonNeumannRecursive(range, new List<long>());
		private static IEnumerable<T> NeighborsVonNeumannRecursive<T>(this T v, long range, IList<long> taken) where T : IVector<T>, new()
		{
			if (taken.Count == v.Count)
			{
				if (taken.Any(static x => x != 0))
				{
					T t = new T();
					for (int i = 0; i < v.Count; i++)
					{
						t[i] = v[i] + taken[(int)v.Count - i - 1];
					}
					yield return t;
				}
			}
			else
			{
				for (long i = -range; i <= range; i++)
				{
					taken.Add(i);
					foreach (T t in v.NeighborsVonNeumannRecursive(range - Math.Abs(i), taken))
					{
						yield return t;
					}
					taken.RemoveAt(taken.Count - 1);
				}
			}
		}

		public static long ManhattanDistance<T>(this T vector, T other) where T : IVector<T>, new()
		{
			long distance = 0L;
			for (long i = 0; i < vector.Count; i++)
			{
				distance += Math.Abs(vector[i] - other[i]);
			}
			return distance;
		}

		public static double Distance<T>(this T vector, T other) where T : IVector<T>, new()
		{
			double distance = 0.0d;
			for (long i = 0; i < vector.Count; i++)
			{
				distance += Math.Pow(vector[i] - other[i], 2);
			}
			return Math.Sqrt(distance);
		}

		public static T Add<T>(this T vector, T other) where T : IVector<T>, new()
		{
			T t = new T();
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = vector[i] + other[i];
			}
			return t;
		}
		public static T Subtract<T>(this T vector, T other) where T : IVector<T>, new()
		{
			T t = new T();
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = vector[i] - other[i];
			}
			return t;
		}
		public static T Multiply<T>(this T vector, long value) where T : IVector<T>, new()
		{
			T t = new T();
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = vector[i] * value;
			}
			return t;
		}
		public static T Divide<T>(this T vector, long value) where T : IVector<T>, new()
		{
			T t = new T();
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = vector[i] / value;
			}
			return t;
		}
		public static T Negate<T>(this T vector) where T : IVector<T>, new()
		{
			T t = new T();
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = -vector[i];
			}
			return t;
		}

		public static bool Equals<T>(T a, T b) where T : IVector<T>, new()
		{
			for (long i = 0; i < a.Count; i++)
			{
				if (a[i] != b[i])
				{
					return false;
				}
			}
			return true;
		}
		public static int Compare<T>(T a, T b) where T : IVector<T>, new()
		{
			for (long i = a.Count - 1; i >= 0; i--)
			{
				int cmp = a[i].CompareTo(b[i]);
				if (cmp != 0)
				{
					return cmp;
				}
			}
			return 0;
		}

		public static string AsString<T>(T vector) where T : IVector<T>, new()
		{
			StringBuilder sb = new StringBuilder("(");
			for (long i = 0; i < vector.Count; i++)
			{
				sb.Append(vector[i]);
				if (i + 1 < vector.Count)
				{
					sb.Append(", ");
				}
			}
			return sb.Append(')').ToString();
		}
	}
}
