record Node(List<Node> children, List<int> metadata);
static Node NewNode() => new(new(), new());
static IEnumerable<int> TraverseMetadata(Node node)
{
	foreach (int m in node.metadata)
	{
		yield return m;
	}
	foreach (int m in node.children.SelectMany(TraverseMetadata))
	{
		yield return m;
	}
}

int[] numbers = File.ReadAllText("input.txt").Split(' ').Select(Int32.Parse).ToArray();
Stack<(Node n, int c, int m)> stack = new Stack<(Node, int, int)>();
stack.Push((NewNode(), numbers[0], numbers[1]));
for (int i = 2; i < numbers.Length; i++)
{
	var (n, c, m) = stack.Pop();
	if (c > 0)
	{
		stack.Push((n, c - 1, m));
		stack.Push((NewNode(), numbers[i], numbers[++i]));
	}
	else if (m > 0)
	{
		stack.Push((n, c, m - 1));
		n.metadata.Add(numbers[i]);
	}
	else
	{
		stack.Peek().n.children.Add(n);
		i--;
	}
}

WriteLine(TraverseMetadata(stack.Single().n).Sum());
