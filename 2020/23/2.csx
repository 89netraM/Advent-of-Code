var cups = new LinkedList<long>(
	File.ReadAllText("input.txt")
		.Select(c => Int64.Parse(c.ToString()))
);
for (int i = 10; i <= 1000000; i++)
{
	cups.AddLast(i);
}
var cupsDict = new Dictionary<long, (bool active, LinkedListNode<long> node)>(cups.Count);
for (var node = cups.First; !(node is null); node = node.Next)
{
	cupsDict.Add(node.Value, (true, node));
}

var current = cups.First;
var pickUp = new LinkedListNode<long>[3];
long next = -1;

for (int i = 0; i < 10000000; i++)
{
	var nextPickUp = current.Next ?? cups.First;
	for (int j = 0; j < 3; j++)
	{
		pickUp[j] = nextPickUp;
		nextPickUp = nextPickUp.Next ?? cups.First;
		cups.Remove(pickUp[j]);
		cupsDict[pickUp[j].Value] = (false, pickUp[j]);
	}

	next = current.Value == 1 ? cups.Count + pickUp.Length : current.Value - 1;
	while (!cupsDict[next].active)
	{
		next = next == 1 ? cups.Count + pickUp.Length : next - 1;
	}
	var index = cupsDict[next].node;
	for (int j = 0; j < pickUp.Length; j++)
	{
		cups.AddAfter(index, pickUp[j]);
		cupsDict[pickUp[j].Value] = (true, pickUp[j]);
		index = pickUp[j];
	}
	current = current.Next ?? cups.First;
}

WriteLine(
	cups.SkipWhile(l => l != 1).Skip(1).Take(2).Aggregate((a, l) => a * l)
);