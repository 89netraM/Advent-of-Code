(bool run, string op, int arg) MakeOp(string line)
{
	string[] spe = line.Split(' ');
	return (false, spe[0], Int32.Parse(spe[1]));
}

var ops = File.ReadAllLines("input.txt").Select(MakeOp).ToArray();
int acc = 0;
int pc = 0;
while (pc < ops.Length)
{
	int tPC = pc;
	var op = ops[pc];

	if (op.run)
	{
		break;
	}
	else if (op.op == "acc")
	{
		acc += op.arg;
		pc++;
	}
	else if (op.op == "jmp")
	{
		pc += op.arg;
	}
	else
	{
		pc++;
	}

	ops[tPC] = (true, op.op, op.arg);
}
WriteLine(acc);