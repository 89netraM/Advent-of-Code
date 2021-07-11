void DealIntoNewStack(IList<int> deck)
{
	for (int i = 0; i < deck.Count / 2; i++)
	{
		(deck[i], deck[deck.Count - (i + 1)]) = (deck[deck.Count - (i + 1)], deck[i]);
	}
}

void Cut(int n, IList<int> deck)
{
	if (n > 0)
	{
		int[] cut = new int[n];
		for (int i = 0; i < deck.Count; i++)
		{
			if (i < n)
			{
				cut[i] = deck[i];
			}
			if (i + n < deck.Count)
			{
				deck[i] = deck[i + n];
			}
			else
			{
				deck[i] = cut[i - (deck.Count - n)];
			}
		}
	}
	else if (n < 0)
	{
		n = Math.Abs(n);
		int[] cut = new int[n];
		for (int i = deck.Count - 1; i >= 0; i--)
		{
			if (i + n >= deck.Count)
			{
				cut[i - (deck.Count - n)] = deck[i];
			}
			if (i - n >= 0)
			{
				deck[i] = deck[i - n];
			}
			else
			{
				deck[i] = cut[i];
			}
		}
	}
}

void DealWithIncrement(int n, IList<int> deck)
{
	int[] og = new int[deck.Count];
	deck.CopyTo(og, 0);
	int pointer = 0;
	for (int i = 0; i < deck.Count; i++)
	{
		deck[pointer] = og[i];
		pointer = (pointer + n) % deck.Count;
	}
}

const int CardCount = 10007;
IList<int> deck = Enumerable.Range(0, CardCount).ToList();
foreach (string line in File.ReadLines("input.txt"))
{
	if (line == "deal into new stack")
	{
		DealIntoNewStack(deck);
	}
	else if (line.StartsWith("cut "))
	{
		Cut(Int32.Parse(line[4..]), deck);
	}
	else if (line.StartsWith("deal with increment "))
	{
		DealWithIncrement(Int32.Parse(line[20..]), deck);
	}
}
WriteLine(deck.IndexOf(2019));
