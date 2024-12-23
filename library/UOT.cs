using System;

namespace AoC.Library
{
	public static class UOT
	{
		public static UOT<T> New<T>(T a, T b, T c) => new(a, b, c);
	}

	public class UOT<T>
	{
		public T A { get; }
		public T B { get; }
		public T C { get; }

		public UOT(T a, T b, T c) =>
			(A, B, C) = (a, b, c);

		public void Deconstruct(out T a, out T b, out T c) =>
			(a, b, c) = (A, B, C);

		public override bool Equals(object? obj) =>
			obj is UOT<T> uot && Equals(uot);

		private bool Equals(UOT<T>? other) =>
			other is not null &&
				((TEquals(A, other.A) && TEquals(B, other.B) && TEquals(C, other.C)) ||
				(TEquals(A, other.A) && TEquals(B, other.C) && TEquals(C, other.B)) ||
				(TEquals(A, other.B) && TEquals(B, other.C) && TEquals(C, other.A)) ||
				(TEquals(A, other.B) && TEquals(B, other.A) && TEquals(C, other.C)) ||
				(TEquals(A, other.C) && TEquals(B, other.A) && TEquals(C, other.B)) ||
				(TEquals(A, other.C) && TEquals(B, other.B) && TEquals(C, other.A)));

		private static bool TEquals(T? a, T? b) =>
			(a is null && b is null) || (a?.Equals(b) ?? false);

		public override int GetHashCode() =>
			(A?.GetHashCode() ?? 0) ^ (B?.GetHashCode() ?? 0) ^ (C?.GetHashCode() ?? 0);

		public override string ToString() =>
			$"{nameof(UOT<T>)} {{ {nameof(A)} = {A}, {nameof(B)} = {B}, {nameof(C)} = {C} }}";

		public static bool operator ==(UOT<T>? a, UOT<T>? b) =>
			(a is null && b is null) || (a?.Equals(b) ?? false);

		public static bool operator !=(UOT<T>? a, UOT<T>? b) =>
			!(a == b);
	}
}
