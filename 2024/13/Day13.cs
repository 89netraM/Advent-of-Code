using System.Linq;
using AoC.Library;
using Microsoft.Z3;
using RegExtract;

namespace AoC.Year2024;

[Day(13)]
public class Day13
{
    [Part(1)]
    public object Part1(string input) =>
        input
            .Paragraphs()
            .AsParallel()
            .Select(p =>
            {
                var lines = p.Lines();
                return new Entry(
                    lines[0].Extract<Vector2>(@"Button A: X\+(\d+), Y\+(\d+)"),
                    lines[1].Extract<Vector2>(@"Button B: X\+(\d+), Y\+(\d+)"),
                    lines[2].Extract<Vector2>(@"Prize: X=(\d+), Y=(\d+)")
                );
            })
            .Sum(e =>
            {
                var list = Enumerable
                    .Range(0, 101)
                    .SelectMany(f => Enumerable.Range(0, 101).Select(s => (f, s)))
                    .Where(p => e.ButtonA * p.f + e.ButtonB * p.s == e.Prize);
                if (list.Any())
                    return list.Min(p => p.f * 3 + p.s);
                return 0L;
            });

    [Part(2)]
    public object Part2(string input) =>
        input
            .Paragraphs()
            .AsParallel()
            .Select(p =>
            {
                var lines = p.Lines();
                return new Entry(
                    lines[0].Extract<Vector2>(@"Button A: X\+(\d+), Y\+(\d+)"),
                    lines[1].Extract<Vector2>(@"Button B: X\+(\d+), Y\+(\d+)"),
                    lines[2].Extract<Vector2>(@"Prize: X=(\d+), Y=(\d+)")
                        + new Vector2(10000000000000, 10000000000000)
                );
            })
            .Sum(e =>
            {
                using var ctx = new Context();
                var optimizer = ctx.MkOptimize();
                var buttonA = ctx.MkIntConst("buttonA");
                var buttonB = ctx.MkIntConst("buttonB");
                optimizer.Assert(
                    ctx.MkEq(buttonA * e.ButtonA.X + buttonB * e.ButtonB.X, ctx.MkInt(e.Prize.X))
                );
                optimizer.Assert(
                    ctx.MkEq(buttonA * e.ButtonA.Y + buttonB * e.ButtonB.Y, ctx.MkInt(e.Prize.Y))
                );
                optimizer.MkMinimize(buttonA * 3 + buttonB);
                return optimizer.Check() is Status.SATISFIABLE
                    ? ((IntNum)optimizer.Model.Eval(buttonA * 3 + buttonB)).Int64
                    : 0L;
            });

    record Entry(Vector2 ButtonA, Vector2 ButtonB, Vector2 Prize);
}
