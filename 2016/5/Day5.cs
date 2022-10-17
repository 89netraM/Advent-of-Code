using System;
using System.Linq;
using AoC.Library;
using System.Security.Cryptography;
using System.Text;

namespace AoC.Year2016;

[Day(5)]
public class Day5
{
	[Part(1)]
	public object Part1(string input)
	{
		var password = "";
		var md5 = MD5.Create();
		for (long l = 0; password.Length < 8; l++)
		{
			var data = Encoding.ASCII.GetBytes($"{input}{l}");
			var hash = String.Concat(md5.ComputeHash(data).Take(3).Select(b => b.ToString("x2")));
			if (hash.StartsWith("00000"))
			{
				password += hash[5];
			}
		}
		return password;
	}

	[Part(2)]
	public object Part2(string input)
	{
		var password = new char[8];
		var md5 = MD5.Create();
		var found = 0b00000000;
		for (long l = 0; found != 0b11111111; l++)
		{
			var data = Encoding.ASCII.GetBytes($"{input}{l}");
			var hash = String.Concat(md5.ComputeHash(data).Take(4).Select(b => b.ToString("x2")));
			if (hash.StartsWith("00000"))
			{
				var index = (int)(hash[5] - '0');
				if (0 <= index && index < 8)
				{
					var id = 0b10000000 >> index;
					if ((found & id) == 0)
					{
						password[index] = hash[6];
						found |= id;
					}
				}
			}
		}
		return String.Concat(password);
	}
}
