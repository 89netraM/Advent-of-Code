namespace AoC.Library
{
	public static class MathM
	{
		public static int Mod(int a, int b) =>
			(a % b + b) % b;
		public static long Mod(long a, long b) =>
			(a % b + b) % b;
	}
}
