// --- Day 16: Chronal Classification ---
// As you see the Elves defend their hot chocolate successfully, you go back to falling through time. This is going to become a problem.
// If you're ever going to return to your own time, you need to understand how this device on your wrist works. You have a little while before you reach your next destination, and with a bit of trial and error, you manage to pull up a programming manual on the device's tiny screen.
// According to the manual, the device has four registers (numbered 0 through 3) that can be manipulated by instructions containing one of 16 opcodes. The registers start with the value 0.
// Every instruction consists of four values: an opcode, two inputs (named A and B), and an output (named C), in that order. The opcode specifies the behavior of the instruction and how the inputs are interpreted. The output, C, is always treated as a register.
// In the opcode descriptions below, if something says "value A", it means to take the number given as A literally. (This is also called an "immediate" value.) If something says "register A", it means to use the number given as A to read from (or write to) the register with that number. So, if the opcode addi adds register A and value B, storing the result in register C, and the instruction addi 0 7 3 is encountered, it would add 7 to the value contained by register 0 and store the sum in register 3, never modifying registers 0, 1, or 2 in the process.
// Many opcodes are similar except for how they interpret their arguments. The opcodes fall into seven general categories:
// Addition:
// addr (add register) stores into register C the result of adding register A and register B. 
// addi (add immediate) stores into register C the result of adding register A and value B. 
// Multiplication:
// mulr (multiply register) stores into register C the result of multiplying register A and register B. 
// muli (multiply immediate) stores into register C the result of multiplying register A and value B. 
// Bitwise AND:
// banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B. 
// bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B. 
// Bitwise OR:
// borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B. 
// bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B. 
// Assignment:
// setr (set register) copies the contents of register A into register C. (Input B is ignored.) 
// seti (set immediate) stores value A into register C. (Input B is ignored.) 
// Greater-than testing:
// gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0. 
// gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0. 
// gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0. 
// Equality testing:
// eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0. 
// eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0. 
// eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0. 
// Unfortunately, while the manual gives the name of each opcode, it doesn't seem to indicate the number. However, you can monitor the CPU to see the contents of the registers before and after instructions are executed to try to work them out. Each opcode has a number from 0 through 15, but the manual doesn't say which is which. For example, suppose you capture the following sample:
// Before: [3, 2, 1, 1]
// 9 2 1 2
// After:  [3, 2, 2, 1]
// This sample shows the effect of the instruction 9 2 1 2 on the registers. Before the instruction is executed, register 0 has value 3, register 1 has value 2, and registers 2 and 3 have value 1. After the instruction is executed, register 2's value becomes 2.
// The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2, B=1, and C=2. Opcode 9 could be any of the 16 opcodes listed above, but only three of them behave in a way that would cause the result shown in the sample:
// Opcode 9 could be mulr: register 2 (which has a value of 1) times register 1 (which has a value of 2) produces 2, which matches the value stored in the output register, register 2. 
// Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1 produces 2, which matches the value stored in the output register, register 2. 
// Opcode 9 could be seti: value 2 matches the value stored in the output register, register 2; the number given for B is irrelevant. 
// None of the other opcodes produce the result captured in the sample. Because of this, the sample above behaves like three opcodes.
// You collect many of these samples (the first section of your puzzle input). The manual also includes a small test program (the second section of your puzzle input) - you can ignore it for now.
// Ignoring the opcode numbers, how many samples in your puzzle input behave like three or more opcodes?
// To begin, get your puzzle input.

record Register(int one, int two, int three, int four);
static Register ParseRegister(string line)
{
	string[] parts = line[9..^1].Split(", ");
	return new Register(Int32.Parse(parts[0]), Int32.Parse(parts[1]), Int32.Parse(parts[2]), Int32.Parse(parts[3]));
}
static int Get(this Register reg, int index) => index switch
{
	0 => reg.one,
	1 => reg.two,
	2 => reg.three,
	3 => reg.four,
	_ => throw new IndexOutOfRangeException(),
};
static Register Set(this Register register, int index, int value) => index switch
{
	0 => register with { one = value },
	1 => register with { two = value },
	2 => register with { three = value },
	3 => register with { four = value },
	_ => throw new IndexOutOfRangeException(),
};

delegate Register Instruction(int a, int b, int c, Register register);

// Addition
static Register Addr(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) + register.Get(b));
static Register Addi(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) + b);

// Multiplication
static Register Mulr(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) * register.Get(b));
static Register Muli(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) * b);

// Bitwise AND
static Register Banr(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) & register.Get(b));
static Register Bani(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) & b);

// Bitwise OR
static Register Borr(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) | register.Get(b));
static Register Bori(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) | b);

// Assignment
static Register Setr(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a));
static Register Seti(int a, int b, int c, Register register) =>
	register.Set(c, a);

// Greater-than testing
static Register Gtir(int a, int b, int c, Register register) =>
	register.Set(c, a > register.Get(b) ? 1 : 0);
static Register Gtri(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) > b ? 1 : 0);
static Register Gtrr(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) > register.Get(b) ? 1 : 0);

// Equality testing
static Register Eqir(int a, int b, int c, Register register) =>
	register.Set(c, a == register.Get(b) ? 1 : 0);
static Register Eqri(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) == b ? 1 : 0);
static Register Eqrr(int a, int b, int c, Register register) =>
	register.Set(c, register.Get(a) == register.Get(b) ? 1 : 0);

Instruction[] instructions = new Instruction[]
{
	Addr,
	Addi,
	Mulr,
	Muli,
	Banr,
	Bani,
	Borr,
	Bori,
	Setr,
	Seti,
	Gtir,
	Gtri,
	Gtrr,
	Eqir,
	Eqri,
	Eqrr,
};

string[] chunks = File.ReadAllText("input.txt").Split("\n\n");
int numberOfThrees = 0;
foreach (string chunk in chunks)
{
	string[] lines = chunk.Split('\n');
	if (lines.Length == 3 && lines[0].StartsWith("Before:") && lines[2].StartsWith("After:"))
	{
		Register before = ParseRegister(lines[0]);
		string[] parts = lines[1].Split(' ');
		int opcode = Int32.Parse(parts[0]);
		int a = Int32.Parse(parts[1]);
		int b = Int32.Parse(parts[2]);
		int c = Int32.Parse(parts[3]);
		Register after = ParseRegister(lines[2]);
		int numberOfOpcodes = 0;
		foreach (Instruction instruction in instructions)
		{
			if (instruction(a, b, c, before) == after)
			{
				numberOfOpcodes++;
			}
		}
		if (numberOfOpcodes >= 3)
		{
			numberOfThrees++;
		}
	}
}

WriteLine(numberOfThrees);
