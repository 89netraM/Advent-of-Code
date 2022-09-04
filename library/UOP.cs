namespace AoC.Library
{
	public static class UOP
	{
		public static UOP<T> New<T>(T a, T b) => new UOP<T>(a, b);
	}

	public class UOP<T>
	{
		public T A { get; }
		public T B { get; }

		public UOP(T a, T b) =>
			(A, B) = (a, b);

		public override bool Equals(object? obj) =>
			obj is UOP<T> oip && Equals(oip);

		private bool Equals(UOP<T>? other) =>
			other is not null &&
				((TEquals(A, other.A) && TEquals(B, other.B)) ||
				(TEquals(A, other.B) && TEquals(B, other.A)));

		private static bool TEquals(T? a, T? b) =>
			(a is null && b is null) || (a?.Equals(b) ?? false);

		public override int GetHashCode() =>
			(A?.GetHashCode() ?? 0) ^ (B?.GetHashCode() ?? 0);

		public override string ToString() =>
			$"{nameof(UOP<T>)} {{ {nameof(A)} = {A}, {nameof(B)} = {B} }}";

		public static bool operator ==(UOP<T>? a, UOP<T>? b) =>
			(a is null && b is null) || (a?.Equals(b) ?? false);

		public static bool operator !=(UOP<T>? a, UOP<T>? b) =>
			!(a == b);
	}
}
