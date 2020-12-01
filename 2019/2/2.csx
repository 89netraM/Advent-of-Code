void RunIntcode(int[] program)
{
	for (int ip = 0; ip < program.Length; ip += 4)
	{
		int op = program[ip];
		switch (op)
		{
			case 1:
				program[program[ip + 3]] = program[program[ip + 1]] + program[program[ip + 2]];
				break;
			case 2:
				program[program[ip + 3]] = program[program[ip + 1]] * program[program[ip + 2]];
				break;
			default:
				WriteLine($"Error: op {op} at {ip}");
				goto default;
			case 99:
				return;
		}
	}
}

int[] original = File.ReadAllText("./input.txt").Split(',').Select(Int32.Parse).ToArray();

for (int noun = 0; noun <= 99; noun++)
{
	for (int verb = 0; verb <= 99; verb++)
	{
		int[] program = (int[])original.Clone();
		program[1] = noun;
		program[2] = verb;
		RunIntcode(program);
		if (program[0] == 19690720)
		{
			WriteLine(100 * noun + verb);
		}
	}
}