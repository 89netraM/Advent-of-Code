using System.Collections.Immutable;

static class Instruction
{
	public static long GetOP(long ins) => ins % 100;

	public static long GetParamCount(long op) => op switch
	{
		1 => 3,
		2 => 3,
		3 => 1,
		4 => 1,
		5 => 2,
		6 => 2,
		7 => 3,
		8 => 3,
		9 => 1
	};

	public static ParamMode GetParamMode(long ins, long pos) => (ParamMode)(ins / (long)Math.Pow(10, pos + 1) % 10);
	public static IEnumerable<ParamMode> GetParamConfigs(long ins, long paramCount)
	{
		for (long p = 1; p <= paramCount; p++)
		{
			yield return GetParamMode(ins, p);
		}
	}

	public static IEnumerable<long> GetParams(State state, IEnumerable<ParamMode> paramConfigs) => paramConfigs.Select((m, i) => m switch
	{
		ParamMode.Position => state.Get(state.PC + 1 + i),
		ParamMode.Immediate => state.PC + 1 + i,
		ParamMode.Relative => state.Base + state.Get(state.PC + 1 + i)
	});
}

readonly struct PCChange
{
	public static PCChange Move { get; } = new PCChange(false, 0);
	public static PCChange Goto(long target) => new PCChange(true, target);

	private bool IsGoto { get; }
	private long Target { get; }

	private PCChange(bool isGoto, long target) => (IsGoto, Target) = (isGoto, target);

	public long NextPC(long currentPC, long paramCount) => IsGoto ? Target : currentPC + paramCount + 1;
}

enum ParamMode : long
{
	Position = 0,
	Immediate = 1,
	Relative = 2
}

delegate long Get();
delegate void Put(long v);
delegate ValueTuple<State, PCChange> InstructionFunction(Get g, Put p, State s, IReadOnlyList<long> ps);

readonly struct State
{
	private static IImmutableDictionary<long, InstructionFunction> Instructions { get; }
	static State()
	{
		var builder = ImmutableDictionary.CreateBuilder<long, InstructionFunction>();
		builder.Add(1, (g, p, s, ps) => (s.Set(ps[2], s.Get(ps[0]) + s.Get(ps[1])), PCChange.Move));
		builder.Add(2, (g, p, s, ps) => (s.Set(ps[2], s.Get(ps[0]) * s.Get(ps[1])), PCChange.Move));
		builder.Add(3, (g, p, s, ps) => (s.Set(ps[0], g()), PCChange.Move));
		builder.Add(4, (g, p, s, ps) => { p(s.Get(ps[0])); return (s, PCChange.Move); });
		builder.Add(5, (g, p, s, ps) => (s, s.Get(ps[0]) != 0 ? PCChange.Goto(s.Get(ps[1])) : PCChange.Move));
		builder.Add(6, (g, p, s, ps) => (s, s.Get(ps[0]) == 0 ? PCChange.Goto(s.Get(ps[1])) : PCChange.Move));
		builder.Add(7, (g, p, s, ps) => (s.Set(ps[2], s.Get(ps[0]) < s.Get(ps[1]) ? 1 : 0), PCChange.Move));
		builder.Add(8, (g, p, s, ps) => (s.Set(ps[2], s.Get(ps[0]) == s.Get(ps[1]) ? 1 : 0), PCChange.Move));
		builder.Add(9, (g, p, s, ps) => (s.SetBase(s.Base + s.Get(ps[0])), PCChange.Move));
		Instructions = builder.ToImmutable();
	}

	public static State Parse(string s) => new State(s.Split(",").Select((v, i) => (i, v: Int64.Parse(v))).ToImmutableDictionary(p => (long)p.i, p => p.v), 0, 0);

	public IImmutableDictionary<long, long> Program { get; }
	public long Base { get; }
	public long PC { get; }

	public State(IImmutableDictionary<long, long> program, long @base, long pc)
	{
		Program = program ?? throw new ArgumentNullException(nameof(program));
		Base = @base;
		PC = pc;
	}

	public long Get(long pos) => Program.TryGetValue(pos, out long v) ? v : 0;
	public State Set(long pos, long value) => new State(Program.SetItem(pos, value), Base, PC);

	public State SetBase(long newBase) => new State(Program, newBase, PC);

	public bool Step(Get get, Put put, out State state)
	{
		long op;
		if (!Program.TryGetValue(PC, out long ins) || (op = Instruction.GetOP(ins)) == 99)
		{
			state = this;
			return false;
		}
		else
		{
			long paramCount = Instruction.GetParamCount(op);
			IReadOnlyList<long> paramPositions = Instruction.GetParams(this, Instruction.GetParamConfigs(ins, paramCount)).ToImmutableList();
			var (newState, pcChange) = Instructions[op](get, put, this, paramPositions);
			state = new State(newState.Program, newState.Base, pcChange.NextPC(PC, paramCount));
			return true;
		}
	}

	public bool StepTo(Get get, Put put, long targetOP, out State state)
	{
		state = this;
		while (state.Get(state.PC) != targetOP && state.Step(get, put, out state)) ;
		return state.Step(get, put, out state);
	}
}

State state = State.Parse(File.ReadAllText("input.txt"));
ISet<(long, long)> map = new HashSet<(long, long)>();
char robotDir = ' ';
(long x, long y) robotPos = (-1, -1);
long x = 0;
long y = 0;
void PutScaffolding(long l)
{
	if (l == 10)
	{
		x = 0;
		y++;
	}
	else
	{
		if (l != 46 && l != 35)
		{
			robotDir = (char)l;
			robotPos = (x, y);
		}
		else if (l != 46)
		{
			map.Add((x, y));
		}
		x++;
	}
}
state.StepTo(() => 0, PutScaffolding, 99, out _);

char Rotation(char from, char to) => (from, to) switch
{
	('^', '>') => 'R',
	('>', 'v') => 'R',
	('v', '<') => 'R',
	('<', '^') => 'R',
	('^', '<') => 'L',
	('<', 'v') => 'L',
	('v', '>') => 'L',
	('>', '^') => 'L',
};
char Direction()
{
	var up = (robotPos.x, robotPos.y - 1);
	if (map.Contains(up) && !visited.Contains(up))
	{
		return '^';
	}
	var left = (robotPos.x - 1, robotPos.y);
	if (map.Contains(left) && !visited.Contains(left))
	{
		return '<';
	}
	var right = (robotPos.x + 1, robotPos.y);
	if (map.Contains(right) && !visited.Contains(right))
	{
		return '>';
	}
	var down = (robotPos.x, robotPos.y + 1);
	if (map.Contains(down) && !visited.Contains(down))
	{
		return 'v';
	}
	return ' ';
}
(long x, long y) Move(char dir) => dir switch
{
	'^' => (robotPos.x, robotPos.y - 1),
	'<' => (robotPos.x - 1, robotPos.y),
	'>' => (robotPos.x + 1, robotPos.y),
	'v' => (robotPos.x, robotPos.y + 1),
};

ISet<(long, long)> visited = new HashSet<(long, long)>();
IList<string> instructions = new List<string>();
while (true)
{
	char dir = Direction();
	if (dir == ' ')
	{
		break;
	}

	instructions.Add(Rotation(robotDir, dir).ToString());
	robotDir = dir;

	long steps = 0;
	var nextPos = Move(robotDir);
	while (map.Contains(nextPos))
	{
		visited.Add(nextPos);
		robotPos = nextPos;
		nextPos = Move(robotDir);
		steps++;
	}
	instructions.Add(steps.ToString());
	map.Add(robotPos);
}

WriteLine(String.Join(",", instructions));

// Instruction sets extracted by hand in `scaffolding.txt`.
string ins = @"A,B,B,A,C,A,C,A,C,B
R,6,R,6,R,8,L,10,L,4
R,6,L,10,R,8
L,4,L,12,R,6,L,10
n
";
int insPC = 0;
long output = -1;
long GetIns() => (long)ins[insPC++];
void PutOutput(long l) => output = l;

new State(state.Program.SetItem(0, 2), state.Base, state.PC).StepTo(GetIns, PutOutput, 99, out _);
WriteLine(output);