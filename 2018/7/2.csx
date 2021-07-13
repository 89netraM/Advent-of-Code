Dictionary<char, List<char>> prerequisites = new Dictionary<char, List<char>>();
SortedSet<char> unfinished = new SortedSet<char>();

foreach (string lines in File.ReadLines("input.txt"))
{
	char a = lines[5];
	char b = lines[36];
	List<char> after;
	if (!prerequisites.TryGetValue(b, out after))
	{
		prerequisites[b] = after = new List<char>();
	}
	after.Add(a);
	unfinished.Add(a);
	unfinished.Add(b);
}

(int time, char task)[] workers = new (int, char)[5];
int time = 0;
while (unfinished.Count > 0)
{
	for (int i = 0; i < workers.Length; i++)
	{
		if (workers[i].time == 0)
		{
			foreach (char task in unfinished)
			{
				if ((!prerequisites.TryGetValue(task, out List<char> pre) || pre.All(p => !unfinished.Contains(p))) &&
					workers.All(w => w.task != task))
				{
					workers[i] = (60 + task - 'A' + 1, task);
					break;
				}
			}
		}
	}
	for (int i = 0; i < workers.Length; i++)
	{
		if (workers[i].time > 1)
		{
			workers[i] = (workers[i].time - 1, workers[i].task);
		}
		else if (workers[i].time > 0)
		{
			unfinished.Remove(workers[i].task);
			workers[i] = default;
		}
	}
	time++;
}
WriteLine(time);
