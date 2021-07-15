// --- Day 13: Mine Cart Madness ---
// A crop of this size requires significant logistics to transport produce, soil, fertilizer, and so on. The Elves are very busy pushing things around in carts on some kind of rudimentary system of tracks they've come up with.
// Seeing as how cart-and-track systems don't appear in recorded history for another 1000 years, the Elves seem to be making this up as they go along. They haven't even figured out how to avoid collisions yet.
// You map out the tracks (your puzzle input) and see where you can help.
// Tracks consist of straight paths (| and -), curves (/ and \), and intersections (+). Curves connect exactly two perpendicular pieces of track; for example, this is a closed loop:
// /----\
// |    |
// |    |
// \----/
// Intersections occur when two perpendicular paths cross. At an intersection, a cart is capable of turning left, turning right, or continuing straight. Here are two loops connected by two intersections:
// /-----\
// |     |
// |  /--+--\
// |  |  |  |
// \--+--/  |
//    |     |
//    \-----/
// Several carts are also on the tracks. Carts always face either up (^), down (v), left (<), or right (>). (On your initial map, the track under each cart is a straight path matching the direction the cart is facing.)
// Each time a cart has the option to turn (by arriving at any intersection), it turns left the first time, goes straight the second time, turns right the third time, and then repeats those directions starting again with left the fourth time, straight the fifth time, and so on. This process is independent of the particular intersection at which the cart has arrived - that is, the cart has no per-intersection memory.
// Carts all move at the same speed; they take turns moving a single step at a time. They do this based on their current location: carts on the top row move first (acting from left to right), then carts on the second row move (again from left to right), then carts on the third row, and so on. Once each cart has moved one step, the process repeats; each of these loops is called a tick.
// For example, suppose there are two carts on a straight track:
// |  |  |  |  |
// v  |  |  |  |
// |  v  v  |  |
// |  |  |  v  X
// |  |  ^  ^  |
// ^  ^  |  |  |
// |  |  |  |  |
// First, the top cart moves. It is facing down (v), so it moves down one square. Second, the bottom cart moves. It is facing up (^), so it moves up one square. Because all carts have moved, the first tick ends. Then, the process repeats, starting with the first cart. The first cart moves down, then the second cart moves up - right into the first cart, colliding with it! (The location of the crash is marked with an X.) This ends the second and last tick.
// Here is a longer example:
// /->-\        
// |   |  /----\
// | /-+--+-\  |
// | | |  | v  |
// \-+-/  \-+--/
//   \------/   
// 
// /-->\        
// |   |  /----\
// | /-+--+-\  |
// | | |  | |  |
// \-+-/  \->--/
//   \------/   
// 
// /---v        
// |   |  /----\
// | /-+--+-\  |
// | | |  | |  |
// \-+-/  \-+>-/
//   \------/   
// 
// /---\        
// |   v  /----\
// | /-+--+-\  |
// | | |  | |  |
// \-+-/  \-+->/
//   \------/   
// 
// /---\        
// |   |  /----\
// | /->--+-\  |
// | | |  | |  |
// \-+-/  \-+--^
//   \------/   
// 
// /---\        
// |   |  /----\
// | /-+>-+-\  |
// | | |  | |  ^
// \-+-/  \-+--/
//   \------/   
// 
// /---\        
// |   |  /----\
// | /-+->+-\  ^
// | | |  | |  |
// \-+-/  \-+--/
//   \------/   
// 
// /---\        
// |   |  /----<
// | /-+-->-\  |
// | | |  | |  |
// \-+-/  \-+--/
//   \------/   
// 
// /---\        
// |   |  /---<\
// | /-+--+>\  |
// | | |  | |  |
// \-+-/  \-+--/
//   \------/   
// 
// /---\        
// |   |  /--<-\
// | /-+--+-v  |
// | | |  | |  |
// \-+-/  \-+--/
//   \------/   
// 
// /---\        
// |   |  /-<--\
// | /-+--+-\  |
// | | |  | v  |
// \-+-/  \-+--/
//   \------/   
// 
// /---\        
// |   |  /<---\
// | /-+--+-\  |
// | | |  | |  |
// \-+-/  \-<--/
//   \------/   
// 
// /---\        
// |   |  v----\
// | /-+--+-\  |
// | | |  | |  |
// \-+-/  \<+--/
//   \------/   
// 
// /---\        
// |   |  /----\
// | /-+--v-\  |
// | | |  | |  |
// \-+-/  ^-+--/
//   \------/   
// 
// /---\        
// |   |  /----\
// | /-+--+-\  |
// | | |  X |  |
// \-+-/  \-+--/
//   \------/   
// After following their respective paths for a while, the carts eventually crash. To help prevent crashes, you'd like to know the location of the first crash. Locations are given in X,Y coordinates, where the furthest left column is X=0 and the furthest top row is Y=0:
//            111
//  0123456789012
// 0/---\        
// 1|   |  /----\
// 2| /-+--+-\  |
// 3| | |  X |  |
// 4\-+-/  \-+--/
// 5  \------/   
// In this example, the location of the first crash is 7,3.

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
		if (carts.TryGetValue(pos, out Cart c))
		{
			Vector nextPos = c.Move(pos);
			if (carts.ContainsKey(nextPos))
			{
				carts.Remove(nextPos);
				positions.Remove(nextPos);
				carts.Remove(pos);
				positions.Remove(pos);
			}
			else
			{
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
	}
	if (positions.Count == 1)
	{
		Vector pos = positions.Single();
		WriteLine($"{pos.x},{pos.y}");
		return;
	}
}
