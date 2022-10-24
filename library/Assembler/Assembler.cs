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

public class Assembler : IEnumerable
{
	public IDictionary<string, Func<Instruction, string>> OpTranspilers { get; }
	public Func<Instruction, string> FallbackTranspiler = ins => throw new ArgumentException($"Can not transpile {ins.Op} instructions");

	public Assembler() =>
		OpTranspilers = new Dictionary<string, Func<Instruction, string>>();

	public Assembler(IDictionary<string, Func<Instruction, string>> opTranspilers) =>
		OpTranspilers = opTranspilers;

	public Assembler(Assembler original) : this()
	{
		foreach (var (op, transpiler) in original.OpTranspilers)
		{
			Add(op, transpiler);
		}
		FallbackTranspiler = original.FallbackTranspiler;
	}

	public void Add(string op, Func<Instruction, string> transpiler) =>
		OpTranspilers.Add(op, transpiler);

	public Action<long[]>? Compile(string input)
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

		code.AppendLine(@"return;
			}");

		return code.ToString();
	}

	private string Transpile(Instruction instruction) =>
		OpTranspilers.TryGetValue(instruction.Op, out var transpiler)
			? transpiler(instruction)
			: FallbackTranspiler(instruction);

	private static Action<long[]>? CompileFunction(string body)
	{
		var code = $"public static class Program {{ public static void Main(long[] {Transpiler.RegisterName}) {{ {body} }} }}";
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
				.CreateDelegate<Action<long[]>>();
		}
		catch (Exception ex)
		{
			Console.Error.WriteLine("Assembler compilation errors:");
			Console.Error.WriteLine($"\t{ex.Message}");
			Console.Error.WriteLine($"C# code:\n{code}");
			return null;
		}
	}

	public IEnumerator GetEnumerator() =>
		OpTranspilers.GetEnumerator();
}
