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

int[] program = File.ReadAllText("./input.txt").Split(',').Select(Int32.Parse).ToArray();
program[1] = 12;
program[2] = 2;
RunIntcode(program);
WriteLine(program[0]);