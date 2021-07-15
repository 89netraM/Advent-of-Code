record Vector(long x, long y);
static Vector ParseDelta(char c) => c switch
{
	'^' => new(0, -1),
	'>' => new(1, 0),
	'v' => new(0, 1),
	'<' => new(-1, 0)
};
class VectorComparer : IComparer<Vector>
{
	public int Compare(Vector a, Vector b)
	{
		int y = a.y.CompareTo(b.y);
		if (y != 0)
		{
			return y;
		}
		else
		{
			return a.x.CompareTo(b.x);
		}
	}
}

enum Direction
{
	Left,
	Straight,
	Right,
}
static Direction Next(this Direction dir) => dir switch
{
	Direction.Left => Direction.Straight,
	Direction.Straight => Direction.Right,
	Direction.Right => Direction.Left,
};

record Cart(Vector delta, Direction dir);
static Cart MakeCart(char c) =>
	new(ParseDelta(c), Direction.Left);
static Vector Move(this Cart c, Vector pos) =>
	new Vector(pos.x + c.delta.x, pos.y + c.delta.y);

delegate Cart Turn(Cart c);
static Cart ForwardTurn(Cart c) =>
	c with { delta = new(-c.delta.y, -c.delta.x) };
static Cart BackwardTurn(Cart c) =>
	c with { delta = new(c.delta.y, c.delta.x) };
static Cart CrossingTurn(Cart c) => new(
	c.dir switch
	{
		Direction.Left => new(c.delta.y, -c.delta.x),
		Direction.Straight => c.delta,
		Direction.Right => new(-c.delta.y, c.delta.x),
	},
	c.dir.Next()
);

SortedSet<Vector> positions = new SortedSet<Vector>(new VectorComparer());
Dictionary<Vector, Turn> map = new Dictionary<Vector, Turn>();
Dictionary<Vector, Cart> carts = new Dictionary<Vector, Cart>();

string[] lines = File.ReadAllLines("input.txt");
for (int y = 0; y < lines.Length; y++)
{
	for (int x = 0; x < lines[y].Length; x++)
	{
		Vector pos = new Vector(x, y);
		switch (lines[y][x])
		{
			case '^':
			case 'v':
			case '<':
			case '>':
				carts[pos] = MakeCart(lines[y][x]);
				positions.Add(pos);
				break;
			case '\\':
				map[pos] = BackwardTurn;
				break;
			case '/':
				map[pos] = ForwardTurn;
				break;
			case '+':
				map[pos] = CrossingTurn;
				break;
		}
	}
}

while (true)
{
	foreach (Vector pos in positions.ToArray())
	{
		Cart c = carts[pos];
		Vector nextPos = c.Move(pos);
		if (carts.ContainsKey(nextPos))
		{
			WriteLine($"{nextPos.x},{nextPos.y}");
			return;
		}
		if (map.TryGetValue(nextPos, out Turn turn))
		{
			c = turn(c);
		}
		carts.Remove(pos);
		positions.Remove(pos);
		carts[nextPos] = c;
		positions.Add(nextPos);
	}
}
