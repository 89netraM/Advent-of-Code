using System;
using System.Text;
using CMD5 = System.Security.Cryptography.MD5;

namespace AoC.Library;

public static class Hash
{
	/// <summary>
	/// Calculates the MD5 hash of the supplied <paramref name="input" /> string.
	/// </summary>
	public static string MD5(string input)
	{
		byte[] data = Encoding.ASCII.GetBytes(input);
		byte[] hash = new byte[CMD5.HashSizeInBytes];

		CMD5.HashData(data, hash);
		return hash.ToHexString();
	}

	/// <summary>
	/// Repeatedly calculates the MD5 hash of the supplied <paramref name="input" /> string.
	/// </summary>
	public static string MD5(string input, int repetitions)
	{
		byte[] data = Encoding.ASCII.GetBytes(input);
		byte[] hash = new byte[CMD5.HashSizeInBytes];

		int length = data.Length;

		for (int i = 0; i < repetitions; i++)
		{
			CMD5.HashData(data.AsSpan().Slice(0, length), hash);
			HashToHexArray(ref data, hash);
			length = hash.Length * 2;
		}

		return hash.ToHexString();

		static void HashToHexArray(ref byte[] data, byte[] hash)
		{
			if (data.Length < hash.Length * 2)
			{
				data = new byte[hash.Length * 2];
			}

			for (int i = 0; i < hash.Length; i++)
			{
				data[i * 2] = IntToHex(hash[i] >> 4);
				data[i * 2 + 1] = IntToHex(hash[i] & 0x0f);
			}
		}

		static byte IntToHex(int n) =>
			(byte)(n < 10 ? (n + '0') : (n - 10 + 'a'));
	}
}
