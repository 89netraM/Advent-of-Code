using System.Collections;
using System.Numerics;

static BigInteger Mod(BigInteger l, BigInteger m) => (l % m + m) % m;
static BigInteger Inv(BigInteger n, BigInteger m) => BigInteger.ModPow(n, m - 2, m);

struct Deck : IEnumerable<BigInteger>
{
	public BigInteger Offset { get; private set; }
	public BigInteger Increment { get; private set; }

	public BigInteger Count { get; }
	public BigInteger this[BigInteger n] => Mod(Offset + Increment * n, Count);

	public Deck(BigInteger count) =>
		(Offset, Increment, Count) = (0, 1, count);
	public Deck(BigInteger offset, BigInteger increment, BigInteger count) =>
		(Offset, Increment, Count) = (offset, increment, count);

	public void DealIntoNewStack()
	{
		Increment = Mod(-Increment, Count);
		Offset = Mod(Offset + Increment, Count);
	}

	public void Cut(BigInteger n)
	{
		Offset = Mod(Offset + Increment * n, Count);
	}

	public void DealWithIncrement(BigInteger n)
	{
		Increment = Mod(Increment * Inv(n, Count), Count);
	}

	public override string ToString() =>
		$"[{nameof(Deck)} {{ {nameof(Offset)} = {Offset}, {nameof(Increment)} = {Increment}, {nameof(Count)} = {Count} }}]";

	public IEnumerator<BigInteger> GetEnumerator()
	{
		for (BigInteger l = 0; l < Count; l++)
		{
			yield return this[l];
		}
	}
	IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

BigInteger CardCount = 119315717514047;
BigInteger Repetitions = 101741582076661;
Deck deck = new Deck(CardCount);
// Do it once
foreach (string line in File.ReadLines("input.txt"))
{
	if (line == "deal into new stack")
	{
		deck.DealIntoNewStack();
	}
	else if (line.StartsWith("cut "))
	{
		deck.Cut(BigInteger.Parse(line[4..]));
	}
	else if (line.StartsWith("deal with increment "))
	{
		deck.DealWithIncrement(BigInteger.Parse(line[20..]));
	}
}
// Calculate end using geometric series
BigInteger increment = BigInteger.ModPow(deck.Increment, Repetitions, CardCount);
BigInteger offset = Mod(deck.Offset * (1 - increment) * Inv(Mod(1 - deck.Increment, CardCount), CardCount), CardCount);
deck = new Deck(
	Mod(deck.Offset * (1 - increment) * Inv(Mod(1 - deck.Increment, CardCount), CardCount), CardCount),
	increment,
	CardCount
);

WriteLine(deck[2020]);
