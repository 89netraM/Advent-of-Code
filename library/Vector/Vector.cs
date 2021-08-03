using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AoC.Library
{
	public static class Vector
	{
		internal static T FromArray<T>(IReadOnlyList<long> array) where T : IVector<T> => array.Count switch
		{
			2 => (T)(object)new Vector2(array[0], array[1]),
			3 => (T)(object)new Vector3(array[0], array[1], array[2]),
			4 => (T)(object)new Vector4(array[0], array[1], array[2], array[3]),
			_ => throw new ArgumentException("Invalid array length"),
		};

		public static T Zero<T>() where T : struct, IVector<T> =>
			default;

		public static IEnumerable<T> DirectionsMoore<T>() where T : struct, IVector<T> =>
			Zero<T>().NeighborsMoore();
		public static IEnumerable<T> DirectionsVonNeumann<T>() where T : struct, IVector<T> =>
			Zero<T>().NeighborsVonNeumann();
		public static IEnumerable<T> Directions<T>(NeighborhoodKind kind) where T : struct, IVector<T> => kind switch
		{
			NeighborhoodKind.Moore => DirectionsMoore<T>(),
			NeighborhoodKind.VonNeumann => DirectionsVonNeumann<T>(),
			_ => throw new ArgumentException("Invalid neighborhood kind"),
		};

		public static IEnumerable<T> NeighborsMoore<T>(this T v, long range = 1) where T : IVector<T> =>
			v.NeighborsMooreRecursive(range, new List<long>());
		private static IEnumerable<T> NeighborsMooreRecursive<T>(this T v, long range, IList<long> taken) where T : IVector<T>
		{
			if (taken.Count == v.Count)
			{
				if (taken.Any(static x => x != 0))
				{
					long[] t = new long[v.Count];
					for (int i = 0; i < v.Count; i++)
					{
						t[i] = v[i] + taken[(int)v.Count - i - 1];
					}
					yield return FromArray<T>(t);
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

		public static IEnumerable<T> NeighborsVonNeumann<T>(this T v, long range = 1) where T : IVector<T> =>
			v.NeighborsVonNeumannRecursive(range, new List<long>());
		private static IEnumerable<T> NeighborsVonNeumannRecursive<T>(this T v, long range, IList<long> taken) where T : IVector<T>
		{
			if (taken.Count == v.Count)
			{
				if (taken.Any(static x => x != 0))
				{
					long[] t = new long[v.Count];
					for (int i = 0; i < v.Count; i++)
					{
						t[i] = v[i] + taken[(int)v.Count - i - 1];
					}
					yield return FromArray<T>(t);
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

		public static IEnumerable<T> Neighbors<T>(this T v, NeighborhoodKind kind, long range = 1) where T : struct, IVector<T> => kind switch
		{
			NeighborhoodKind.Moore => v.NeighborsMoore(range),
			NeighborhoodKind.VonNeumann => v.NeighborsVonNeumann(range),
			_ => throw new ArgumentException("Invalid neighborhood kind"),
		};

		public static long ManhattanDistance<T>(this T vector, in T other) where T : IVector<T>
		{
			long distance = 0L;
			for (long i = 0; i < vector.Count; i++)
			{
				distance += Math.Abs(vector[i] - other[i]);
			}
			return distance;
		}

		public static double Distance<T>(this T vector, in T other) where T : IVector<T>
		{
			double distance = 0.0d;
			for (long i = 0; i < vector.Count; i++)
			{
				distance += Math.Pow(vector[i] - other[i], 2);
			}
			return Math.Sqrt(distance);
		}

		public static T Add<T>(this T vector, in T other) where T : IVector<T>
		{
			long[] t = new long[vector.Count];
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = vector[i] + other[i];
			}
			return FromArray<T>(t);
		}
		public static T Subtract<T>(this T vector, in T other) where T : IVector<T>
		{
			long[] t = new long[vector.Count];
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = vector[i] - other[i];
			}
			return FromArray<T>(t);
		}
		public static T Multiply<T>(this T vector, long value) where T : IVector<T>
		{
			long[] t = new long[vector.Count];
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = vector[i] * value;
			}
			return FromArray<T>(t);
		}
		public static T Divide<T>(this T vector, long value) where T : IVector<T>
		{
			long[] t = new long[vector.Count];
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = vector[i] / value;
			}
			return FromArray<T>(t);
		}
		public static T Negate<T>(this T vector) where T : IVector<T>
		{
			long[] t = new long[vector.Count];
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = -vector[i];
			}
			return FromArray<T>(t);
		}

		public static T MinParts<T>(this T vector, in T other) where T : IVector<T>
		{
			long[] t = new long[vector.Count];
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = Math.Min(vector[i], other[i]);
			}
			return FromArray<T>(t);
		}
		public static T MaxParts<T>(this T vector, in T other) where T : IVector<T>
		{
			long[] t = new long[vector.Count];
			for (long i = 0; i < vector.Count; i++)
			{
				t[i] = Math.Max(vector[i], other[i]);
			}
			return FromArray<T>(t);
		}

		public static bool Equals<T>(in T a, in T b) where T : IVector<T>
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
		public static int Compare<T>(in T a, in T b) where T : IVector<T>
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

		public static string AsString<T>(in T vector) where T : IVector<T>
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
