// --- Day 21: Chronal Conversion ---
// You should have been watching where you were going, because as you wander the new North Pole base, you trip and fall into a very deep hole!
// Just kidding. You're falling through time again.
// If you keep up your current pace, you should have resolved all of the temporal anomalies by the next time the device activates. Since you have very little interest in browsing history in 500-year increments for the rest of your life, you need to find a way to get back to your present time.
// After a little research, you discover two important facts about the behavior of the device:
// First, you discover that the device is hard-wired to always send you back in time in 500-year increments. Changing this is probably not feasible.
// Second, you discover the activation system (your puzzle input) for the time travel module. Currently, it appears to run forever without halting.
// If you can cause the activation system to halt at a specific moment, maybe you can make the device send you so far back in time that you cause an integer underflow in time itself and wrap around back to your current time!
// The device executes the program as specified in manual section one and manual section two.
// Your goal is to figure out how the program works and cause it to halt. You can only control register 0; every other register begins at 0 as usual.
// Because time travel is a dangerous activity, the activation system begins with a few instructions which verify that bitwise AND (via bani) does a numeric operation and not an operation as if the inputs were interpreted as strings. If the test fails, it enters an infinite loop re-running the test instead of allowing the program to execute normally. If the test passes, the program continues, and assumes that all other bitwise operations (banr, bori, and borr) also interpret their inputs as numbers. (Clearly, the Elves who wrote this system were worried that someone might introduce a bug while trying to emulate this system with a scripting language.)
// What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the fewest instructions? (Executing the same instruction multiple times counts as multiple instructions executed.)
// To begin, get your puzzle input.

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

static IReadOnlyDictionary<string, Instruction> Instructions = new Dictionary<string, Instruction>
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

string[] input = File.ReadAllLines("input.txt");
int ip = Int32.Parse(input[0].Substring(4));
(Instruction, int, int, int)[] program = input.Skip(1)
	.Select(static l =>
	{
		string[] parts = l.Split(' ');
		return (Instructions[parts[0]], Int32.Parse(parts[1]), Int32.Parse(parts[2]), Int32.Parse(parts[3]));
	})
	.ToArray();

long last = -1;
HashSet<long> seen = new HashSet<long>();
Register registers = new Register(0, 0, 0, 0, 0, 0);
for (int i = 0; i < program.Length; i++)
{
	if (i == 28)
	{
		if (!seen.Add(registers.two))
		{
			Console.WriteLine(last);
			return;
		}
		last = registers.two;
	}

	registers = registers.Set(ip, i);
	var (inst, a, b, c) = program[i];
	registers = inst(a, b, c, registers);
	i = (int)registers.Get(ip);
}
