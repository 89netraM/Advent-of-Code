using System;
using System.Globalization;
using System.Text.RegularExpressions;
using static AoC.Library.Functional;

namespace AoC.Library
{
	public static partial class Parsing
	{
		public static Func<string, T?> PatternParser<T>(string pattern) =>
			PatternParser<T, T>(new Regex(pattern), Id);
		public static Func<string, TO?> PatternParser<TI, TO>(string pattern, Func<TI, TO> transformer) =>
			PatternParser<TI, TO>(new Regex(pattern), transformer);
		public static Func<string, T?> PatternParser<T>(Regex pattern) =>
			PatternParser<T, T>(pattern, Id);
		public static Func<string, TO?> PatternParser<TI, TO>(Regex pattern, Func<TI, TO> transformer)
		{
			if (pattern.GetGroupNumbers().Length != 2)
			{
				throw new ArgumentException("Pattern must have exactly one group.", nameof(pattern));
			}

			return input =>
			{
				Match match = pattern.Match(input);
				if (match.Success && match.Groups[1].Success)
				{
					if (Convert.ChangeType(match.Groups[1].Value, GetType<TI>(), CultureInfo.InvariantCulture) is TI value)
					{
						return transformer(value);
					}
					else
					{
						throw new FormatException($"Could not convert the group (\"{match.Groups[1].Value}\") to {GetType<TI>().Name}. From \"{input}\".");
					}
				}
				else
				{
					return default;
				}
			};
		}

		public static Func<string, (T1, T2)?> PatternParser<T1, T2>(string pattern) =>
			PatternParser<T1, T2, (T1, T2)?>(new Regex(pattern), static (v1, v2) => (v1, v2));
		public static Func<string, TO?> PatternParser<T1, T2, TO>(string pattern, Func<T1, T2, TO> combiner) =>
			PatternParser<T1, T2, TO>(new Regex(pattern), combiner);
		public static Func<string, (T1, T2)?> PatternParser<T1, T2>(Regex pattern) =>
			PatternParser<T1, T2, (T1, T2)?>(pattern, static (v1, v2) => (v1, v2));
		public static Func<string, TO?> PatternParser<T1, T2, TO>(Regex pattern, Func<T1, T2, TO> combiner)
		{
			if (pattern.GetGroupNumbers().Length != 3)
			{
				throw new ArgumentException("Pattern must have exactly two groups.", nameof(pattern));
			}

			return input =>
			{
				Match match = pattern.Match(input);
				if (match.Success && match.Groups[1].Success && match.Groups[2].Success)
				{
					if (Convert.ChangeType(match.Groups[1].Value, GetType<T1>(), CultureInfo.InvariantCulture) is T1 value1)
					{
						if (Convert.ChangeType(match.Groups[2].Value, GetType<T2>(), CultureInfo.InvariantCulture) is T2 value2)
						{
							return combiner(value1, value2);
						}
						else
						{
							throw new FormatException($"Could not convert group 2 (\"{match.Groups[2].Value}\") to {GetType<T2>().Name}. From \"{input}\".");
						}
					}
					else
					{
						throw new FormatException($"Could not convert group 1 (\"{match.Groups[1].Value}\") to {GetType<T1>().Name}. From \"{input}\".");
					}
				}
				else
				{
					return default;
				}
			};
		}

		public static Func<string, (T1, T2, T3)?> PatternParser<T1, T2, T3>(string pattern) =>
			PatternParser<T1, T2, T3, (T1, T2, T3)?>(new Regex(pattern), static (v1, v2, v3) => (v1, v2, v3));
		public static Func<string, TO?> PatternParser<T1, T2, T3, TO>(string pattern, Func<T1, T2, T3, TO> combiner) =>
			PatternParser<T1, T2, T3, TO>(new Regex(pattern), combiner);
		public static Func<string, (T1, T2, T3)?> PatternParser<T1, T2, T3>(Regex pattern) =>
			PatternParser<T1, T2, T3, (T1, T2, T3)?>(pattern, static (v1, v2, v3) => (v1, v2, v3));
		public static Func<string, TO?> PatternParser<T1, T2, T3, TO>(Regex pattern, Func<T1, T2, T3, TO> combiner)
		{
			if (pattern.GetGroupNumbers().Length != 4)
			{
				throw new ArgumentException("Pattern must have exactly three groups.", nameof(pattern));
			}

			return input =>
			{
				Match match = pattern.Match(input);
				if (match.Success && match.Groups[1].Success && match.Groups[2].Success &&
					match.Groups[3].Success)
				{
					if (Convert.ChangeType(match.Groups[1].Value, GetType<T1>(), CultureInfo.InvariantCulture) is T1 value1)
					{
						if (Convert.ChangeType(match.Groups[2].Value, GetType<T2>(), CultureInfo.InvariantCulture) is T2 value2)
						{
							if (Convert.ChangeType(match.Groups[3].Value, GetType<T3>(), CultureInfo.InvariantCulture) is T3 value3)
							{
								return combiner(value1, value2, value3);
							}
							else
							{
								throw new FormatException($"Could not convert group 3 (\"{match.Groups[3].Value}\") to {GetType<T3>().Name}. From \"{input}\".");
							}
						}
						else
						{
							throw new FormatException($"Could not convert group 2 (\"{match.Groups[2].Value}\") to {GetType<T2>().Name}. From \"{input}\".");
						}
					}
					else
					{
						throw new FormatException($"Could not convert group 1 (\"{match.Groups[1].Value}\") to {GetType<T1>().Name}. From \"{input}\".");
					}
				}
				else
				{
					return default;
				}
			};
		}

		public static Func<string, (T1, T2, T3, T4)?> PatternParser<T1, T2, T3, T4>(string pattern) =>
			PatternParser<T1, T2, T3, T4, (T1, T2, T3, T4)?>(new Regex(pattern), static (v1, v2, v3, v4) => (v1, v2, v3, v4));
		public static Func<string, TO?> PatternParser<T1, T2, T3, T4, TO>(string pattern, Func<T1, T2, T3, T4, TO> combiner) =>
			PatternParser<T1, T2, T3, T4, TO>(new Regex(pattern), combiner);
		public static Func<string, (T1, T2, T3, T4)?> PatternParser<T1, T2, T3, T4>(Regex pattern) =>
			PatternParser<T1, T2, T3, T4, (T1, T2, T3, T4)?>(pattern, static (v1, v2, v3, v4) => (v1, v2, v3, v4));
		public static Func<string, TO?> PatternParser<T1, T2, T3, T4, TO>(Regex pattern, Func<T1, T2, T3, T4, TO> combiner)
		{
			if (pattern.GetGroupNumbers().Length != 5)
			{
				throw new ArgumentException("Pattern must have exactly four groups.", nameof(pattern));
			}

			return input =>
			{
				Match match = pattern.Match(input);
				if (match.Success && match.Groups[1].Success && match.Groups[2].Success &&
					match.Groups[3].Success && match.Groups[4].Success)
				{
					if (Convert.ChangeType(match.Groups[1].Value, GetType<T1>(), CultureInfo.InvariantCulture) is T1 value1)
					{
						if (Convert.ChangeType(match.Groups[2].Value, GetType<T2>(), CultureInfo.InvariantCulture) is T2 value2)
						{
							if (Convert.ChangeType(match.Groups[3].Value, GetType<T3>(), CultureInfo.InvariantCulture) is T3 value3)
							{
								if (Convert.ChangeType(match.Groups[4].Value, GetType<T4>(), CultureInfo.InvariantCulture) is T4 value4)
								{
									return combiner(value1, value2, value3, value4);
								}
								else
								{
									throw new FormatException($"Could not convert group 4 (\"{match.Groups[4].Value}\") to {GetType<T4>().Name}. From \"{input}\".");
								}
							}
							else
							{
								throw new FormatException($"Could not convert group 3 (\"{match.Groups[3].Value}\") to {GetType<T3>().Name}. From \"{input}\".");
							}
						}
						else
						{
							throw new FormatException($"Could not convert group 2 (\"{match.Groups[2].Value}\") to {GetType<T2>().Name}. From \"{input}\".");
						}
					}
					else
					{
						throw new FormatException($"Could not convert group 1 (\"{match.Groups[1].Value}\") to {GetType<T1>().Name}. From \"{input}\".");
					}
				}
				else
				{
					return default;
				}
			};
		}

		private static Type GetType<T>()
		{
			Type t = typeof(T);
			if (t.Name == "Nullable`1")
			{
				t = t.GenericTypeArguments[0];
			}
			return t;
		}
	}
}
