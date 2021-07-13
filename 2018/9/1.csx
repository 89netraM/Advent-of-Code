static LinkedListNode<T> NextCircularly<T>(this LinkedListNode<T> current) =>
	current.Next is LinkedListNode<T> next ? next : current.List.First;
static LinkedListNode<T> PreviousCircularly<T>(this LinkedListNode<T> current) =>
	current.Previous is LinkedListNode<T> previous ? previous : current.List.Last;

string[] input = File.ReadAllText("input.txt").Split(" players; last marble is worth ");
int[] scores = new int[Int32.Parse(input[0])];
int marbles = Int32.Parse(input[1][..^7]);
int next = 0;
LinkedList<int> circle = new LinkedList<int>();
LinkedListNode<int> current = circle.AddFirst(next++);

for (int i = 0; i < marbles; i++)
{
	if (next % 23 == 0)
	{
		LinkedListNode<int> toRemove = current;
		for (int j = 0; j < 7; j++)
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
