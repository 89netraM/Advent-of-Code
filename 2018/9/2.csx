static LinkedListNode<T> NextCircularly<T>(this LinkedListNode<T> current) =>
	current.Next is LinkedListNode<T> next ? next : current.List.First;
static LinkedListNode<T> PreviousCircularly<T>(this LinkedListNode<T> current) =>
	current.Previous is LinkedListNode<T> previous ? previous : current.List.Last;

string[] input = File.ReadAllText("input.txt").Split(" players; last marble is worth ");
long[] scores = new long[Int64.Parse(input[0])];
long marbles = Int64.Parse(input[1][..^7]) * 100;
long next = 0;
LinkedList<long> circle = new LinkedList<long>();
LinkedListNode<long> current = circle.AddFirst(next++);

for (long i = 0; i < marbles; i++)
{
	if (next % 23 == 0)
	{
		LinkedListNode<long> toRemove = current;
		for (long j = 0; j < 7; j++)
		{
			toRemove = toRemove.PreviousCircularly();
		}
		current = toRemove.NextCircularly();
		circle.Remove(toRemove);
		scores[i % scores.Length] += next++ + toRemove.Value;
	}
	else
	{
		current = circle.AddAfter(current.NextCircularly(), next++);
	}
}

WriteLine(scores.Max());
