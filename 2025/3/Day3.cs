using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using ComputeSharp;
using System.Runtime.Versioning;
using System.Runtime.InteropServices;

namespace AoC.Year2025;

[Day(3)]
[SupportedOSPlatform("windows")]
public partial class Day3
{
	[Part(1)]
	public object Part1(string input)
	{
		var lines = input.Lines();
		var height = lines.Length;
		var width = lines[0].Length;
		var banks = lines
			.SelectMany(l => l.Select(c => c - '0'))
			.ToArray();

		var graphicsDevice = GraphicsDevice.GetDefault();
		var result = new R8[height];
		using (var banksBuffer = graphicsDevice.AllocateReadOnlyTexture2D(banks, width, height))
		using (var resultBuffer = graphicsDevice.AllocateReadWriteTexture2D<R8, float>(1, height))
		{
			graphicsDevice.ForEach(resultBuffer, new Part1ComputeShader(banksBuffer));
			resultBuffer.CopyTo(result);
		}
		return result.Sum(p => (long)p.R);
	}

	[ThreadGroupSize(DefaultThreadGroupSizes.Y)]
	[GeneratedComputeShaderDescriptor]
	public readonly partial struct Part1ComputeShader(ReadOnlyTexture2D<int> texture) : IComputeShader<float>
	{
		public float Execute()
		{
			var max = 0;
			var maxI = 0;
			for (var i = 0; i < texture.Width - 1; i++)
			{
				if (texture[i, ThreadIds.Y] > max)
				{
					max = texture[i, ThreadIds.Y];
					maxI = i;
				}
			}
			var max2 = 0;
			for (var i = maxI + 1; i < texture.Width; i++)
			{
				if (texture[i, ThreadIds.Y] > max2)
				{
					max2 = texture[i, ThreadIds.Y];
				}
			}
			return (max * 10 + max2) / 256.0f;
		}
	}

	[Part(2)]
	public object Part2(string input)
	{
		var lines = input.Lines();
		var height = lines.Length;
		var width = lines[0].Length;
		var banks = lines
			.SelectMany(l => l.Select(c => (uint)(c - '0')))
			.ToArray();

		var graphicsDevice = GraphicsDevice.GetDefault();
		var result = new uint[height, 2];
		using (var banksBuffer = graphicsDevice.AllocateReadOnlyTexture2D(banks, width, height))
		using (var resultBuffer = graphicsDevice.AllocateReadWriteTexture2D<uint>(2, height))
		{
			graphicsDevice.For(height, new Part2ComputeShader(banksBuffer, resultBuffer));
			resultBuffer.CopyTo(result);
		}
		var sum = 0L;
		for (var i = 0; i < height; i++)
		{
			sum += result[i, 0] * 10000000L + result[i, 1];
		}
		return sum;
	}

	[ThreadGroupSize(DefaultThreadGroupSizes.Y)]
	[GeneratedComputeShaderDescriptor]
	public readonly partial struct Part2ComputeShader(ReadOnlyTexture2D<uint> texture, ReadWriteTexture2D<uint> result) : IComputeShader
	{
		public void Execute()
		{
			uint a = 0;
			int prevIndex = 0;
			for (var n = 11; n >= 7; n--)
			{
				var nextIndex = FindMax(ThreadIds.X, prevIndex, texture.Width - prevIndex - n);
				a = a * 10 + texture[nextIndex, ThreadIds.X];
				prevIndex = nextIndex + 1;
			}
			result[0, ThreadIds.X] = a;
			uint b = 0;
			for (var n = 6; n >= 0; n--)
			{
				var nextIndex = FindMax(ThreadIds.X, prevIndex, texture.Width - prevIndex - n);
				b = b * 10 + texture[nextIndex, ThreadIds.X];
				prevIndex = nextIndex + 1;
			}
			result[1, ThreadIds.X] = b;
		}

		private int FindMax(int y, int offset, int length)
		{
			uint max = texture[offset, y];
			int maxI = offset;
			for (var i = 1; i < length; i++)
			{
				if (texture[offset + i, y] > max)
				{
					max = texture[offset + i, y];
					maxI = offset + i;
				}
			}
			return maxI;
		}
	}
}
