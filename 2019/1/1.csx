int RequiredFuel(int mass) => Math.Max(0, (mass / 3) - 2);

WriteLine(File.ReadAllLines("./input.txt").Select(Int32.Parse).Sum(RequiredFuel));