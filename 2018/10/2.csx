record Vector(long x, long y);
static Vector ParseVector(string s)
{
	string[] ss = s.Split(',');
	return new Vector(Int64.Parse(ss[0].Trim()), Int64.Parse(ss[1].Trim()));
}
record Light { public Vector pos { get; set; } public Vector vel { get; set; } };
static Light ParseLight(string line)
{
	int firstGt = line.IndexOf('>');
	return new Light
	{
		pos = ParseVector(line[10..firstGt]),
		vel = ParseVector(line[(line.IndexOf('<', firstGt) + 1)..line.IndexOf('>', firstGt + 1)])
	};
}

static void StepLights(Light[] lights)
{
	foreach (Light light in lights)
	{
		light.pos = new(light.pos.x + light.vel.x, light.pos.y + light.vel.y);
	}
}
static void ReverseLights(Light[] lights)
{
	foreach (Light light in lights)
	{
		light.pos = new(light.pos.x - light.vel.x, light.pos.y - light.vel.y);
	}
}

static (long, long, long, long) BoundingBox(Light[] lights) =>
	lights.Aggregate((minX: Int64.MaxValue, minY: Int64.MaxValue, maxX: Int64.MinValue, maxY: Int64.MinValue), static (a, l) => (Math.Min(a.minX, l.pos.x), Math.Min(a.minY, l.pos.y), Math.Max(a.maxX, l.pos.x), Math.Max(a.maxY, l.pos.y)));
static void WriteLights(Light[] lights)
{
	var (minX, minY, maxX, maxY) = BoundingBox(lights);
	for (long y = minY; y <= maxY; y++)
	{
		for (long x = minX; x <= maxX; x++)
		{
			Write(lights.Any(l => l.pos.x == x && l.pos.y == y) ? '#' : '.');
		}
		WriteLine();
	}
}

Light[] lights = File.ReadLines("input.txt").Select(ParseLight).ToArray();
long area = Int64.MaxValue;
for (long i = 0; true; i++)
{
	StepLights(lights);
	var (minX, minY, maxX, maxY) = BoundingBox(lights);
	long newArea = (maxX - minX) * (maxY - minY);
	if (newArea > area)
	{
		WriteLine(i);
		break;
	}
	area = newArea;
}
