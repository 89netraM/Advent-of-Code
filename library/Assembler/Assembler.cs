using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace AoC.Library;

public abstract class AssemblerBase : IEnumerable
{
	public IDictionary<string, Func<Transpiler, Instruction, string>> OpTranspilers { get; }
	public Func<Transpiler, Instruction, string> FallbackTranspiler { get; set; } = (_, ins) => throw new ArgumentException($"Can not transpile {ins.Op} instructions");
	public Transpiler Transpiler { get; init; }

	public IReadOnlyList<string> ArgNames { get; init; } = new string[0];
	public string Header { get; init; } = "";
	public string Footer { get; init; } = "";

	public AssemblerBase(Transpiler transpiler) =>
		(Transpiler, OpTranspilers) = (transpiler, new Dictionary<string, Func<Transpiler, Instruction, string>>());

	public AssemblerBase(Transpiler transpiler, IDictionary<string, Func<Transpiler, Instruction, string>> opTranspilers) =>
		(Transpiler, OpTranspilers) = (transpiler, opTranspilers);

	public AssemblerBase(AssemblerBase original)
	{
		OpTranspilers = new Dictionary<string, Func<Transpiler, Instruction, string>>();
		Transpiler = original.Transpiler;
		foreach (var (op, transpiler) in original.OpTranspilers)
		{
			this[op] = transpiler;
		}
		FallbackTranspiler = original.FallbackTranspiler;
	}

	public Func<Transpiler, Instruction, string> this[string op]
	{
		get => OpTranspilers[op];
		set => OpTranspilers[op] = value;
	}

	public IEnumerator GetEnumerator() =>
		OpTranspilers.GetEnumerator();
}

public class Assembler<TFunc> : AssemblerBase
	where TFunc : Delegate
{
	public Assembler(Transpiler transpiler) : base(transpiler) { }

	public Assembler(Transpiler transpiler, IDictionary<string, Func<Transpiler, Instruction, string>> opTranspilers) : base(transpiler, opTranspilers) { }

	public Assembler(AssemblerBase original) : base(original) { }

	public TFunc? Compile(string input)
	{
		var code = BuildCode(input);
		return CompileFunction(code);
	}

	private string BuildCode(string input)
	{
		var code = new StringBuilder();
		code.AppendLine(@$"
			long {Transpiler.InstructionPointerName} = 0;
			{Transpiler.StartLabel}:
			switch ({Transpiler.InstructionPointerName})
			{{");

		foreach (var (i, line) in input.Lines().Enumerate())
		{
			if (i != 0)
			{
				code.AppendLine($"goto case {i};");
			}
			code.AppendLine($"case {i}:");
			code.AppendLine($"{Transpiler.InstructionPointerName} = {i};");

			var instruction = new Instruction(line);
			code.AppendLine(Transpile(instruction));
		}

		code.AppendLine(@"
				break;
			}");

		return code.ToString();
	}

	private string Transpile(Instruction instruction) =>
		OpTranspilers.TryGetValue(instruction.Op, out var transpiler)
			? transpiler(Transpiler, instruction)
			: FallbackTranspiler(Transpiler, instruction);

	private TFunc? CompileFunction(string body)
	{
		var delegateType = typeof(TFunc);
		var returnType = GetReturnTypeFromDelegate(delegateType);
		var arguments = GetArgumentTypesFromDelegate(delegateType)
			.Zip(ArgNames, (t, n) => $"{t} {n}")
			.Let(args => String.Join(", ", args));
		var code = @$"public static class Program
			{{
				public static {returnType} Main({arguments})
				{{
					{Transpiler.Header}
					{Header}
					{body}
					{Footer}
				}}
			}}";
		var compilation = CSharpCompilation.Create(
			Path.GetRandomFileName(),
			new[] { CSharpSyntaxTree.ParseText(code) },
			new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) },
			new CSharpCompilationOptions(
				outputKind: OutputKind.DynamicallyLinkedLibrary,
				optimizationLevel: OptimizationLevel.Release,
				checkOverflow: true));

		var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
		if (errors.Length > 0)
		{
			Console.Error.WriteLine("Assembler compilation errors:");
			foreach (var error in errors)
			{
				Console.Error.WriteLine($"\t{error.GetMessage()}");
			}
			Console.Error.WriteLine($"C# code:\n{code}");
			return null;
		}

		using var ms = new MemoryStream();
		compilation.Emit(ms);
		try
		{
			var compiledAssembly = Assembly.Load(ms.ToArray());
			return compiledAssembly
				.GetType("Program")?
				.GetMethod("Main")?
				.CreateDelegate<TFunc>();
		}
		catch (Exception ex)
		{
			Console.Error.WriteLine("Assembler compilation errors:");
			Console.Error.WriteLine($"\t{ex.Message}");
			Console.Error.WriteLine($"C# code:\n{code}");
			return null;
		}
	}

	private static string GetReturnTypeFromDelegate(Type delegateType) =>
		GetMethodInfoOfDelegate(delegateType)
			.ReturnType
			.Let(GetCodeNameOfType);

	private static IEnumerable<string> GetArgumentTypesFromDelegate(Type delegateType) =>
		GetMethodInfoOfDelegate(delegateType)
			.GetParameters()
			.Select(pi => GetCodeNameOfType(pi.ParameterType));

	private static MethodInfo GetMethodInfoOfDelegate(Type delegateType) =>
		delegateType
			.GetMethod("Invoke")!;

	private static string GetCodeNameOfType(Type type)
	{
		if (!type.IsGenericType)
		{
			var typeName = type.FullName!;
			var infoIndex = typeName.IndexOf("[");
			if (infoIndex == -1)
			{
				return typeName;
			}
			else
			{
				return typeName.Substring(0, infoIndex);
			}
		}
		else
		{
			var typeName = type.FullName!;
			typeName = typeName.Substring(0, typeName.IndexOf("`"));
			var typeArgs = type.GetGenericArguments()
				.Select(GetCodeNameOfType)
				.Let(args => String.Join(", ", args));
			return $"{typeName}<{typeArgs}>";
		}
	}
}
