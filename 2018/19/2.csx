record Register(long one, long two, long three, long four, long five, long six);
static long Get(this Register reg, long index) => index switch
{
	0 => reg.one,
	1 => reg.two,
	2 => reg.three,
	3 => reg.four,
	4 => reg.five,
	5 => reg.six,
	_ => throw new IndexOutOfRangeException(),
};
static Register Set(this Register register, long index, long value) => index switch
{
	0 => register with { one = value },
	1 => register with { two = value },
	2 => register with { three = value },
	3 => register with { four = value },
	4 => register with { five = value },
	5 => register with { six = value },
	_ => throw new IndexOutOfRangeException(),
};

delegate Register Instruction(long a, long b, long c, Register register);

// Addition
static Register Addr(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) + register.Get(b));
static Register Addi(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) + b);

// Multiplication
static Register Mulr(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) * register.Get(b));
static Register Muli(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) * b);

// Bitwise AND
static Register Banr(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) & register.Get(b));
static Register Bani(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) & b);

// Bitwise OR
static Register Borr(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) | register.Get(b));
static Register Bori(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) | b);

// Assignment
static Register Setr(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a));
static Register Seti(long a, long b, long c, Register register) =>
	register.Set(c, a);

// Greater-than testing
static Register Gtir(long a, long b, long c, Register register) =>
	register.Set(c, a > register.Get(b) ? 1 : 0);
static Register Gtri(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) > b ? 1 : 0);
static Register Gtrr(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) > register.Get(b) ? 1 : 0);

// Equality testing
static Register Eqir(long a, long b, long c, Register register) =>
	register.Set(c, a == register.Get(b) ? 1 : 0);
static Register Eqri(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) == b ? 1 : 0);
static Register Eqrr(long a, long b, long c, Register register) =>
	register.Set(c, register.Get(a) == register.Get(b) ? 1 : 0);

IReadOnlyDictionary<string, Instruction> Instructions = new Dictionary<string, Instruction>
{
	["addr"] = Addr,
	["addi"] = Addi,
	["mulr"] = Mulr,
	["muli"] = Muli,
	["banr"] = Banr,
	["bani"] = Bani,
	["borr"] = Borr,
	["bori"] = Bori,
	["setr"] = Setr,
	["seti"] = Seti,
	["gtir"] = Gtir,
	["gtri"] = Gtri,
	["gtrr"] = Gtrr,
	["eqir"] = Eqir,
	["eqri"] = Eqri,
	["eqrr"] = Eqrr,
};

string[] program = File.ReadAllLines("input.txt");
int ip = Int32.Parse(program[0].Substring(4));
program = program.Skip(1).ToArray();
Register registers = new Register(1, 0, 0, 0, 0, 0);

for (int i = 0; i < program.Length; i++)
{
	registers = registers.Set(ip, i);
	if (i == 2 && registers.Get(3) != 0)
	{
		if (registers.Get(4) % registers.Get(3) == 0)
		{
			registers = registers.Set(0, registers.Get(0) + registers.Get(3));
		}
		registers = registers.Set(1, registers.Get(4));
		i = 11;
	}
	else
	{
		string[] parts = program[i].Split(' ');
		string op = parts[0];
		int a = Int32.Parse(parts[1]);
		int b = Int32.Parse(parts[2]);
		int c = Int32.Parse(parts[3]);
		registers = Instructions[op](a, b, c, registers);
		i = (int)registers.Get(ip);
	}
}

WriteLine(registers.one);
