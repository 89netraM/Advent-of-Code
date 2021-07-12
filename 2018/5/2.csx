static string OneReaction(string chain)
{
	for (int i = 1; i < chain.Length; i++)
	{
		char f = chain[i - 1];
		char s = chain[i];
		if (Char.ToUpper(f) == Char.ToUpper(s) && Char.IsUpper(f) != Char.IsUpper(s))
		{
			return chain[..(i - 1)] + chain[(i + 1)..];
		}
	}
	return chain;
}
static string AllReactions(string chain)
{
	while (true)
	{
		string nextChain = OneReaction(chain);
		if (nextChain.Length == chain.Length)
		{
			return nextChain;
		}
		else
		{
			chain = nextChain;
		}
	}
}

string chain = File.ReadAllText("input.txt");
int shortest = chain.Length;
for (char i = 'A'; i <= 'Z'; i++)
{
	string collapsed = AllReactions(String.Concat(chain.Where(c => Char.ToUpper(c) != i)));
	shortest = Math.Min(shortest, collapsed.Length);
}
WriteLine(shortest);