int RequiredFuel(int mass)
{
	int moduleFuel = Math.Max(0, (mass / 3) - 2);
	return moduleFuel > 0 ? moduleFuel + RequiredFuel(moduleFuel) : 0;
}

WriteLine(File.ReadAllLines("./input.txt").Select(Int32.Parse).Sum(RequiredFuel));