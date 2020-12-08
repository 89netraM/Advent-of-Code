(string op, int arg) MakeOp(string line)
{
	string[] spe = line.Split(' ');
	return (spe[0], Int32.Parse(spe[1]));
}
var ops = File.ReadAllLines("input.txt").Select(MakeOp).ToArray();
int? RunOps()
{
	HashSet<int> run = new HashSet<int>();
	int acc = 0;
	int pc = 0;
	while (pc < ops.Length)
	{
		int tPC = pc;
		var op = ops[pc];

		if (run.Contains(pc))
		{
			return null;
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
		run.Add(tPC);
	}
	return acc;
}
for (int i = 0; i < ops.Length; i++)
{
	var op = ops[i];
	if (op.op == "nop")
	{
		ops[i] = ("jmp", op.arg);
		int? result = RunOps();
		if (result.HasValue)
		{
			WriteLine(result.Value);
			break;
		}
	}
	else if (op.op == "jmp")
	{
		ops[i] = ("nop", op.arg);
		int? result = RunOps();
		if (result.HasValue)
		{
			WriteLine(result.Value);
			break;
		}
	}
	ops[i] = op;
}