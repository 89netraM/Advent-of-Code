record Coord(int x, int y);
static IEnumerable<Coord> GetNeighbors(this Coord coord)
{
	yield return new Coord(coord.x, coord.y - 1);
	yield return new Coord(coord.x, coord.y + 1);
	yield return new Coord(coord.x + 1, coord.y);
	yield return new Coord(coord.x - 1, coord.y);
}

delegate Coord Visitor(char c, Coord coord);

interface IStep
{
	IEnumerable<Coord> VisitSteps(Visitor visitor, Coord coord);
}
class Direction : IStep
{
	public char DirectionChar { get; }

	public Direction(char direction) =>
		DirectionChar = direction;

	public IEnumerable<Coord> VisitSteps(Visitor visitor, Coord coord)
	{
		yield return visitor(DirectionChar, coord);
	}

	public override string ToString() =>
		DirectionChar.ToString();
}
class Empty : IStep
{
	public Empty() { }

	public IEnumerable<Coord> VisitSteps(Visitor visitor, Coord coord) =>
		new Coord[0];

	public override string ToString() => "";
}
class Alternative : IStep
{
	public IStep Step1 { get; }
	public IStep Step2 { get; }

	public Alternative(IStep step1, IStep step2) =>
		(Step1, Step2) = (step1, step2);

	public IEnumerable<Coord> VisitSteps(Visitor visitor, Coord coord)
	{
		foreach (Coord c in Step1.VisitSteps(visitor, coord))
		{
			yield return c;
		}
		foreach (Coord c in Step2.VisitSteps(visitor, coord))
		{
			yield return c;
		}
	}

	public override string ToString() =>
		$"({Step1}|{Step2})";
}
class Sequence : IStep
{
	public IStep Step1 { get; }
	public IStep Step2 { get; }

	public Sequence(IStep step1, IStep step2) =>
		(Step1, Step2) = (step1, step2);

	public IEnumerable<Coord> VisitSteps(Visitor visitor, Coord coord)
	{
		foreach (Coord c in Step1.VisitSteps(visitor, coord))
		{
			foreach (Coord c2 in Step2.VisitSteps(visitor, c))
			{
				yield return c2;
			}
		}
	}

	public override string ToString() =>
		$"{Step1}{Step2}";
}
static (IStep, ReadOnlyMemory<char>) ParseAlternative(ReadOnlyMemory<char> memory)
{
	IStep step2;
	(step2, memory) = ParseSequence(memory);

	if (step2 is null)
	{
		return (null, memory);
	}
	else if (memory.Length == 0)
	{
		return (step2, memory);
	}
	else if (memory.Span[^1] == '(')
	{
		return (step2, memory.Slice(0, memory.Length - 1));
	}
	else if (memory.Span[^1] == '|')
	{
		IStep step1;
		(step1, memory) = ParseAlternative(memory.Slice(0, memory.Length - 1));

		if (step1 is not null)
		{
			return (new Alternative(step1, step2), memory);
		}
		else
		{
			return (null, memory);
		}
	}
	else
	{
		return (step2, memory);
	}
}
static (IStep, ReadOnlyMemory<char>) ParseSequence(ReadOnlyMemory<char> memory)
{
	IStep step2;
	if (memory.Span[^1] == ')')
	{
		(step2, memory) = ParseAlternative(memory.Slice(0, memory.Length - 1));
	}
	else
	{
		step2 = memory.Span[^1] switch
		{
			'N' => new Direction('N'),
			'S' => new Direction('S'),
			'E' => new Direction('E'),
			'W' => new Direction('W'),
			_ => null,
		};
		if (step2 is null)
		{
			return (new Empty(), memory);
		}
		memory = memory.Slice(0, memory.Length - 1);
	}

	if (step2 is null)
	{
		return (null, memory);
	}
	else if (memory.Length == 0 || memory.Span[^1] == '|' || memory.Span[^1] == '(')
	{
		return (step2, memory);
	}
	else
	{
		IStep step1;
		(step1, memory) = ParseSequence(memory);

		if (step1 is not null)
		{
			return (new Sequence(step1, step2), memory);
		}
		else
		{
			return (null, memory);
		}
	}
}

ReadOnlyMemory<char> input = File.ReadAllText("input.txt").Trim().ToCharArray();

var (step, _) = ParseAlternative(input.Slice(1, input.Length - 2));
Coord start = new Coord(0, 0);
HashSet<Coord> map = new HashSet<Coord> { start };
_ = step.VisitSteps(
	(direction, coord) =>
	{
		Coord move = direction switch
		{
			'N' => new Coord(0, -1),
			'S' => new Coord(0, 1),
			'E' => new Coord(1, 0),
			'W' => new Coord(-1, 0),
			_ => throw new ArgumentOutOfRangeException(),
		};
		Coord door = new Coord(coord.x + move.x, coord.y + move.y);
		map.Add(door);
		Coord next = new Coord(door.x + move.x, door.y + move.y);
		map.Add(next);
		return next;
	},
	start
).ToArray();

Dictionary<Coord, int> visited = new Dictionary<Coord, int>();
Queue<(Coord, int)> toVisit = new Queue<(Coord, int)>();
toVisit.Enqueue((start, 0));

while (toVisit.Count > 0)
{
	var (current, distance) = toVisit.Dequeue();
	if (!visited.ContainsKey(current))
	{
		visited.Add(current, distance);
		foreach (Coord neighbor in current.GetNeighbors())
		{
			if (map.Contains(neighbor))
			{
				toVisit.Enqueue((neighbor, distance + 1));
			}
		}
	}
}

WriteLine(visited.Values.Max() / 2);
