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

string chain = File.ReadAllText("input.txt");
while (true)
{
	string nextChain = OneReaction(chain);
	if (nextChain.Length == chain.Length)
	{
		WriteLine(nextChain.Length);
		return;
	}
	else
	{
		chain = nextChain;
	}
}
