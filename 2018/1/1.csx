WriteLine(
	File.ReadAllLines("input.txt")
		.Select(Int64.Parse)
		.Sum()
);
