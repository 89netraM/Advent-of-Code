using System;
using System.Linq;
using AoC.Library;

namespace AoC.Year2017
{
	[Day(20)]
	public class Day20
	{
		private static Func<string, Vector3> vectorParser = Parsing.PatternParser<long, long, long, Vector3>(@"(-?\d+),(-?\d+),(-?\d+)", static (x, y, z) => new Vector3(x, y, z));
		record Particle(Vector3 pos, Vector3 vel, Vector3 acc);

		[Part(1)]
		public object Part1(string input)
		{
			var particles = input.Lines()
				.Select(Parsing.PatternParser<string, string, string, Particle>(
					@"p=<(-?\d+,-?\d+,-?\d+)>, v=<(-?\d+,-?\d+,-?\d+)>, a=<(-?\d+,-?\d+,-?\d+)>",
					static (p, v, a) => new Particle(vectorParser(p), vectorParser(v), vectorParser(a))
				))
				.ToArray();

			for (int i = 0; i < 1000; i++)
			{
				for (int j = 0; j < particles.Length; j++)
				{
					var particle = particles[j];
					particles[j] = particle with {
						pos = particle.pos + particle.vel,
						vel = particle.vel + particle.acc,
					};
				}
			}

			return particles.Select(static (p, i) => (p, i)).Aggregate(static (a, b) => Vector3.Zero.ManhattanDistance(a.p.pos) < Vector3.Zero.ManhattanDistance(b.p.pos) ? a : b).i;
		}
	}
}
