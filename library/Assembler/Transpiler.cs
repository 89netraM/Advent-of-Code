namespace AoC.Library;

public abstract class Transpiler
{
	public const string StartLabel = "START";
	public const string RegisterName = "regs";
	public const string InstructionPointerName = "ip";

	/// <summary>
	/// Instantiates a transpiler where registers are single character variables starting from <paramref name="startRegister" />.
	/// </summary>
	public static Transpiler SingleCharRegister(char startRegister) =>
		new SingleCharRegisterTranspiler(startRegister);

	/// <summary>
	/// Assigns the value of <paramref name="from" /> to <paramref name="to" />.
	/// </summary>
	public string Assign(Instruction ins, int to, int from) =>
		Assign(ins, to, ArgumentToExpression(ins, from));
	/// <summary>
	/// Assigns <paramref name="expression" /> to <paramref name="register" />.
	/// </summary>
	public string Assign(Instruction ins, int register, string expression) =>
		Assign(ArgumentToExpression(ins, register), expression);
	/// <summary>
	/// Assigns <paramref name="expression" /> to <paramref name="variable" />.
	/// </summary>
	public string Assign(string variable, string expression) =>
		$"{variable} = {expression};";

	/// <summary>
	/// Increments the <paramref name="argument" /> by one.
	/// </summary>
	public string Inc(Instruction ins, int argument) =>
		Inc(ArgumentToExpression(ins, argument));
	/// <summary>
	/// Increments the <paramref name="variable" /> by one.
	/// </summary>
	public string Inc(string variable) =>
		$"{variable}++;";

	/// <summary>
	/// Decrements the <paramref name="argument" /> by one.
	/// </summary>
	public string Dec(Instruction ins, int argument) =>
		Dec(ArgumentToExpression(ins, argument));
	/// <summary>
	/// Decrements the <paramref name="variable" /> by one.
	/// </summary>
	public string Dec(string variable) =>
		$"{variable}--;";

	/// <summary>
	/// Adds the values of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="a" />.
	/// </summary>
	public string Add(Instruction ins, int a, int b) =>
		Add(ins, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Adds the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="argument" />.
	/// </summary>
	public string Add(Instruction ins, int argument, string expression) =>
		ArgumentToExpression(ins, argument)
			.Let(arg => Add(arg, arg, expression));
	/// <summary>
	/// Adds the values of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Add(Instruction ins, int to, int a, int b) =>
		Add(ins, to, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Adds the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Add(Instruction ins, int to, int argument, string expression) =>
		Add(ins, to, ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Adds <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Add(Instruction ins, int to, string a, string b) =>
		Add(ArgumentToExpression(ins, to), a, b);
	/// <summary>
	/// Adds <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Add(string to, string a, string b) =>
		$"{to} = {a} + {b};";

	/// <summary>
	/// Subtracts the value of <paramref name="b" /> from <paramref name="a" /> and stores the result in <paramref name="a" />.
	/// </summary>
	public string Sub(Instruction ins, int a, int b) =>
		Sub(ins, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Subtracts <paramref name="expression" /> from the value of <paramref name="argument" /> and stores the result in <paramref name="argument" />.
	/// </summary>
	public string Sub(Instruction ins, int argument, string expression) =>
		ArgumentToExpression(ins, argument)
			.Let(arg => Sub(arg, arg, expression));
	/// <summary>
	/// Subtracts the value of <paramref name="b" /> from <paramref name="a" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Sub(Instruction ins, int to, int a, int b) =>
		Sub(ins, to, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Subtracts <paramref name="expression" /> from the value of <paramref name="argument" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Sub(Instruction ins, int to, int argument, string expression) =>
		Sub(ins, to, ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Subtracts <paramref name="b" /> from <paramref name="a" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Sub(Instruction ins, int to, string a, string b) =>
		Sub(ArgumentToExpression(ins, to), a, b);
	/// <summary>
	/// Subtracts <paramref name="b" /> from <paramref name="a" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Sub(string to, string a, string b) =>
		$"{to} = {a} - {b};";

	/// <summary>
	/// Multiplies the values of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="a" />.
	/// </summary>
	public string Mul(Instruction ins, int a, int b) =>
		Mul(ins, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Multiplies the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="argument" />.
	/// </summary>
	public string Mul(Instruction ins, int argument, string expression) =>
		ArgumentToExpression(ins, argument)
			.Let(arg => Mul(arg, arg, expression));
	/// <summary>
	/// Multiplies the values of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Mul(Instruction ins, int to, int a, int b) =>
		Mul(ins, to, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Multiplies the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Mul(Instruction ins, int to, int argument, string expression) =>
		Mul(ins, to, ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Multiplies <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Mul(Instruction ins, int to, string a, string b) =>
		Mul(ArgumentToExpression(ins, to), a, b);
	/// <summary>
	/// Multiplies <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Mul(string to, string a, string b) =>
		$"{to} = {a} * {b};";

	/// <summary>
	/// Divides the value of <paramref name="a" /> by <paramref name="b" /> and stores the result in <paramref name="a" />.
	/// </summary>
	public string Div(Instruction ins, int a, int b) =>
		Div(ins, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Divides the value of <paramref name="argument" /> by <paramref name="expression" /> and stores the result in <paramref name="argument" />.
	/// </summary>
	public string Div(Instruction ins, int argument, string expression) =>
		ArgumentToExpression(ins, argument)
			.Let(arg => Div(arg, arg, expression));
	/// <summary>
	/// Divides the value of <paramref name="a" /> by <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Div(Instruction ins, int to, int a, int b) =>
		Div(ins, to, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Divides the value of <paramref name="argument" /> by <paramref name="expression" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Div(Instruction ins, int to, int argument, string expression) =>
		Div(ins, to, ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Divides <paramref name="a" /> by <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Div(Instruction ins, int to, string a, string b) =>
		Div(ArgumentToExpression(ins, to), a, b);
	/// <summary>
	/// Divides <paramref name="a" /> by <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Div(string to, string a, string b) =>
		$"{to} = {a} / {b};";

	/// <summary>
	/// Remainder of dividing the value of <paramref name="a" /> by <paramref name="b" /> and stores the result in <paramref name="a" />.
	/// </summary>
	public string Mod(Instruction ins, int a, int b) =>
		Mod(ins, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Remainder of dividing the value of <paramref name="argument" /> by <paramref name="expression" /> and stores the result in <paramref name="argument" />.
	/// </summary>
	public string Mod(Instruction ins, int argument, string expression) =>
		ArgumentToExpression(ins, argument)
			.Let(arg => Mod(arg, arg, expression));
	/// <summary>
	/// Remainder of dividing the value of <paramref name="a" /> by <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Mod(Instruction ins, int to, int a, int b) =>
		Mod(ins, to, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Remainder of dividing the value of <paramref name="argument" /> by <paramref name="expression" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Mod(Instruction ins, int to, int argument, string expression) =>
		Mod(ins, to, ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Remainder of dividing <paramref name="a" /> by <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Mod(Instruction ins, int to, string a, string b) =>
		Mod(ArgumentToExpression(ins, to), a, b);
	/// <summary>
	/// Remainder of dividing <paramref name="a" /> by <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string Mod(string to, string a, string b) =>
		$"{to} = {a} % {b};";

	/// <summary>
	/// Bitwise and between the value of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="a" />.
	/// </summary>
	public string BitAnd(Instruction ins, int a, int b) =>
		BitAnd(ins, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Bitwise and between the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="argument" />.
	/// </summary>
	public string BitAnd(Instruction ins, int argument, string expression) =>
		ArgumentToExpression(ins, argument)
			.Let(arg => BitAnd(arg, arg, expression));
	/// <summary>
	/// Bitwise and between the value of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitAnd(Instruction ins, int to, int a, int b) =>
		BitAnd(ins, to, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Bitwise and between the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitAnd(Instruction ins, int to, int argument, string expression) =>
		BitAnd(ins, to, ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Bitwise and between <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitAnd(Instruction ins, int to, string a, string b) =>
		BitAnd(ArgumentToExpression(ins, to), a, b);
	/// <summary>
	/// Bitwise and between <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitAnd(string to, string a, string b) =>
		$"{to} = {a} & {b};";

	/// <summary>
	/// Bitwise or between the value of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="a" />.
	/// </summary>
	public string BitOr(Instruction ins, int a, int b) =>
		BitOr(ins, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Bitwise or between the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="argument" />.
	/// </summary>
	public string BitOr(Instruction ins, int argument, string expression) =>
		ArgumentToExpression(ins, argument)
			.Let(arg => BitOr(arg, arg, expression));
	/// <summary>
	/// Bitwise or between the value of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitOr(Instruction ins, int to, int a, int b) =>
		BitOr(ins, to, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Bitwise or between the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitOr(Instruction ins, int to, int argument, string expression) =>
		BitOr(ins, to, ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Bitwise or between <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitOr(Instruction ins, int to, string a, string b) =>
		BitOr(ArgumentToExpression(ins, to), a, b);
	/// <summary>
	/// Bitwise or between <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitOr(string to, string a, string b) =>
		$"{to} = {a} | {b};";

	/// <summary>
	/// Bitwise xor between the value of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="a" />.
	/// </summary>
	public string BitXor(Instruction ins, int a, int b) =>
		BitXor(ins, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Bitwise xor between the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="argument" />.
	/// </summary>
	public string BitXor(Instruction ins, int argument, string expression) =>
		ArgumentToExpression(ins, argument)
			.Let(arg => BitXor(arg, arg, expression));
	/// <summary>
	/// Bitwise xor between the value of <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitXor(Instruction ins, int to, int a, int b) =>
		BitXor(ins, to, a, ArgumentToExpression(ins, b));
	/// <summary>
	/// Bitwise xor between the value of <paramref name="argument" /> and <paramref name="expression" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitXor(Instruction ins, int to, int argument, string expression) =>
		BitXor(ins, to, ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Bitwise xor between <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitXor(Instruction ins, int to, string a, string b) =>
		BitXor(ArgumentToExpression(ins, to), a, b);
	/// <summary>
	/// Bitwise xor between <paramref name="a" /> and <paramref name="b" /> and stores the result in <paramref name="to" />.
	/// </summary>
	public string BitXor(string to, string a, string b) =>
		$"{to} = {a} ^ {b};";

	/// <summary>
	/// Iff the <paramref name="condition" /> expression is <see langword="true"/> the <paramref name="then" />
	/// statement(s) are executed.
	/// </summary>
	public string If(string condition, string then) =>
		@$"if ({condition})
		{{
			{then}
		}}";

	/// <summary>
	/// If the <paramref name="condition" /> expression is <see langword="true"/> the <paramref name="then" />
	/// statement(s) are executed, and if not the <paramref name="otherwise" /> statement(s) are.
	/// </summary>
	public string IfElse(string condition, string then, string otherwise) =>
		@$"if ({condition})
		{{
			{then}
		}}
		else
		{{
			{otherwise}
		}}";

	/// <summary>
	/// Adds the value of <paramref name="argument" /> to the instruction pointer and continues from there.
	/// </summary>
	public string JumpOffset(Instruction ins, int argument) =>
		JumpOffset(ArgumentToExpression(ins, argument));
	/// <summary>
	/// Adds <paramref name="offset" /> to the instruction pointer and continues from there.
	/// </summary>
	public string JumpOffset(string offset) =>
		@$"{InstructionPointerName} += {offset};
		goto {StartLabel};";

	/// <summary>
	/// Jumps directly to the instruction pointed to by the <paramref name="argument" />.
	/// </summary>
	public string JumpTo(Instruction ins, int argument) =>
		JumpTo(ArgumentToExpression(ins, argument));
	/// <summary>
	/// Jumps directly to the instruction pointed to by <paramref name="target" />.
	/// </summary>
	public string JumpTo(string target) =>
		@$"{InstructionPointerName} = {target};
		goto {StartLabel};";

	/// <summary>
	/// Grater than or equal comparison between the two arguments <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Ge(Instruction ins, int a, int b) =>
		Ge(ArgumentToExpression(ins, a), ArgumentToExpression(ins, b));
	/// <summary>
	/// Grater than or equal comparison between an <paramref name="argument" /> and an expression <paramref name="expression" />.
	/// </summary>
	public string Ge(Instruction ins, int argument, string expression) =>
		Ge(ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Grater than or equal comparison between the two expressions <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Ge(string a, string b) =>
		$"({a} >= {b})";

	/// <summary>
	/// Grater than comparison between the two arguments <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Gt(Instruction ins, int a, int b) =>
		Gt(ArgumentToExpression(ins, a), ArgumentToExpression(ins, b));
	/// <summary>
	/// Grater than comparison between an <paramref name="argument" /> and an expression <paramref name="expression" />.
	/// </summary>
	public string Gt(Instruction ins, int argument, string expression) =>
		Gt(ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Grater than comparison between the two expressions <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Gt(string a, string b) =>
		$"({a} > {b})";

	/// <summary>
	/// Less than or equal comparison between the two arguments <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Le(Instruction ins, int a, int b) =>
		Le(ArgumentToExpression(ins, a), ArgumentToExpression(ins, b));
	/// <summary>
	/// Less than or equal comparison between an <paramref name="argument" /> and an expression <paramref name="expression" />.
	/// </summary>
	public string Le(Instruction ins, int argument, string expression) =>
		Le(ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Less than or equal comparison between the two expressions <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Le(string a, string b) =>
		$"({a} <= {b})";

	/// <summary>
	/// Less than comparison between the two arguments <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Lt(Instruction ins, int a, int b) =>
		Lt(ArgumentToExpression(ins, a), ArgumentToExpression(ins, b));
	/// <summary>
	/// Less than comparison between an <paramref name="argument" /> and an expression <paramref name="expression" />.
	/// </summary>
	public string Lt(Instruction ins, int argument, string expression) =>
		Lt(ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Less than comparison between the two expressions <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Lt(string a, string b) =>
		$"({a} < {b})";

	/// <summary>
	/// Equality between the two arguments <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Eq(Instruction ins, int a, int b) =>
		Eq(ArgumentToExpression(ins, a), ArgumentToExpression(ins, b));
	/// <summary>
	/// Equality between an <paramref name="argument" /> and an expression <paramref name="expression" />.
	/// </summary>
	public string Eq(Instruction ins, int argument, string expression) =>
		Eq(ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Equality between the two expressions <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Eq(string a, string b) =>
		$"({a} == {b})";

	/// <summary>
	/// Inequality between the two arguments <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Ne(Instruction ins, int a, int b) =>
		Ne(ArgumentToExpression(ins, a), ArgumentToExpression(ins, b));
	/// <summary>
	/// Inequality between an <paramref name="argument" /> and an expression <paramref name="expression" />.
	/// </summary>
	public string Ne(Instruction ins, int argument, string expression) =>
		Ne(ArgumentToExpression(ins, argument), expression);
	/// <summary>
	/// Inequality between the two expressions <paramref name="a" /> and <paramref name="b" />.
	/// </summary>
	public string Ne(string a, string b) =>
		$"({a} != {b})";

	/// <summary>
	/// Returns the argument at <paramref name="index" /> either as a number or a reference to a register.
	/// </summary>
	public string ArgumentToExpression(Instruction ins, int index) =>
		ins.TryNumber(index, out long value)
			? value.ToString()
			: $"{RegisterName}[{RegisterNameToIndex(ins[index])}]";

	/// <summary>
	/// Translates a register name to an index in the register array.
	/// </summary>
	public abstract int RegisterNameToIndex(string registerName);
}

internal class SingleCharRegisterTranspiler : Transpiler
{
	public char StartRegister;

	public SingleCharRegisterTranspiler(char startRegister) =>
		StartRegister = startRegister;

	/// <summary>
	/// Register offset is calculated based on a single character register name starting from <paramref name="startRegister" />.
	/// </summary>
	public override int RegisterNameToIndex(string registerName) =>
		registerName[0] - StartRegister;
}
