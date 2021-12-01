using System.Security.Cryptography;
using System.Text;
using AoC.Library;

namespace AoC.Year2015
{
	[Day(4)]
	public class Day4
	{
		[Part(1)]
		public object Part1(string input)
		{
			MD5 md5 = MD5.Create();
			long number = 100000L;
			while (true)
			{
				var hash = md5.ComputeHash(Encoding.ASCII.GetBytes(input + number));
				if (hash[0] == 0 && hash[1] == 0 && (hash[2] & 0xF0) == 0)
				{
					return number;
				}
				number++;
			}
		}

		[Part(2)]
		public object Part2(string input)
		{
			MD5 md5 = MD5.Create();
			long number = 100000L;
			while (true)
			{
				var hash = md5.ComputeHash(Encoding.ASCII.GetBytes(input + number));
				if (hash[0] == 0 && hash[1] == 0 && hash[2] == 0)
				{
					return number;
				}
				number++;
			}
		}
	}
}
