using System;

namespace AoC.Year2021
{
	public partial class Day24
	{
		static (long x, long y, long z, long w) InputUnprocessed(long a, long b, long c, long d, long e, long f, long g, long h, long i, long j, long k, long l, long m, long n)
		{
			long x = 0L, y = 0L, z = 0L, w = 0L;

			w = a;
			x *= 0;
			x += z;
			x %= 26;
			z /= 1;
			x += 11;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 6;
			y *= x;
			z += y;

			w = b;
			x *= 0;
			x += z;
			x %= 26;
			z /= 1;
			x += 11;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 14;
			y *= x;
			z += y;

			w = c;
			x *= 0;
			x += z;
			x %= 26;
			z /= 1;
			x += 15;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 13;
			y *= x;
			z += y;

			w = d;
			x *= 0;
			x += z;
			x %= 26;
			z /= 26;
			x += -14;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 1;
			y *= x;
			z += y;

			w = e;
			x *= 0;
			x += z;
			x %= 26;
			z /= 1;
			x += 10;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 6;
			y *= x;
			z += y;

			w = f;
			x *= 0;
			x += z;
			x %= 26;
			z /= 26;
			x += 0;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 13;
			y *= x;
			z += y;

			w = g;
			x *= 0;
			x += z;
			x %= 26;
			z /= 26;
			x += -6;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 6;
			y *= x;
			z += y;

			w = h;
			x *= 0;
			x += z;
			x %= 26;
			z /= 1;
			x += 13;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 3;
			y *= x;
			z += y;

			w = i;
			x *= 0;
			x += z;
			x %= 26;
			z /= 26;
			x += -3;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 8;
			y *= x;
			z += y;

			w = j;
			x *= 0;
			x += z;
			x %= 26;
			z /= 1;
			x += 13;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 14;
			y *= x;
			z += y;

			w = k;
			x *= 0;
			x += z;
			x %= 26;
			z /= 1;
			x += 15;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 4;
			y *= x;
			z += y;

			w = l;
			x *= 0;
			x += z;
			x %= 26;
			z /= 26;
			x += -2;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 7;
			y *= x;
			z += y;

			w = m;
			x *= 0;
			x += z;
			x %= 26;
			z /= 26;
			x += -9;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 15;
			y *= x;
			z += y;

			w = n;
			x *= 0;
			x += z;
			x %= 26;
			z /= 26;
			x += -2;
			x = x == w ? 1 : 0;
			x = x == 0 ? 1 : 0;
			y *= 0;
			y += 25;
			y *= x;
			y += 1;
			z *= y;
			y *= 0;
			y += w;
			y += 1;
			y *= x;
			z += y;

			return (x, y, z, w);
		}

		static (long x, long y, long z, long w) Input(long a, long b, long c, long d, long e, long f, long g, long h, long i, long j, long k, long l, long m, long n)
		{
			long x = 0L, y = 0L, z = 0L, w = 0L;

			// a = n - 4
			// b = g - 8
			z = (a + 6L) * 26L + b + 14L;

			// c = d + 1
			if (c - 1L != d)
			{
				z = z * 26L + d + 1L;
			}

			// e = f - 6
			if (e + 6L != f)
			{
				z = z * 26L + f + 13L;
			}

			// z = (n + 2) * 26 + g + 6
			z = Math.DivRem(z, 26L, out x);
			if (x - 6L != g)
			{
				z = z * 26L + g + 6L;
			}

			// h = i
			if (h != i)
			{
				z = z * 26L + i + 8L;
			}

			// z = n + 2
			// j = m - 5
			z = z * 26L + j + 14L;

			// k = l - 2
			if (k + 2L != l)
			{
				z = z * 26L + l + 7L;
			}

			// z = (n + 2) * 26 + m + 9
			z = Math.DivRem(z, 26L, out x);
			if (x - 9L != m)
			{
				z = z * 26L + m + 15L;
			}

			// z = n + 2
			z = Math.DivRem(z, 26L, out x);
			if (x - 2L != n)
			{
				z = z * 26L + n + 1L;
			}

			// For correct output
			x = x - 2L == n ? 0L : 1L;
			y = (n + 1L) * x;
			w = n;

			return (x, y, z, w);
		}
	}
}
