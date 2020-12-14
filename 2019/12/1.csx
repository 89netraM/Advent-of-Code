#load ".\Vector.csx"
#load ".\Body.csx"
#load ".\Universe.csx"

Universe u = new Universe(
	File.ReadAllLines("input.txt")
		.Select(l => new Body(Vector.Parse(l)))
);

for (int i = 0; i < 1000; i++)
{
	u = u.Step();
}

WriteLine(u.CalculateEnergy());