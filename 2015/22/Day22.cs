using System;
using System.Collections.Generic;
using System.Linq;
using AoC.Library;
using System.Text;

namespace AoC.Year2015
{
	[Day(22)]
	public class Day22
	{
		[Part(-1)]
		public object PartN1(string input)
		{
			var start = new State(14)
			{
				HP = 10,
				Mana = 250,
			};

			BFS.Search(start, s => s.StepTurns(8), s => s.BossHP <= 0, out var path);
			Console.WriteLine(start);
			foreach (var (action, step, cost) in path)
			{
				Console.WriteLine($"Make {action} for a total of {cost}");
				Console.WriteLine(step);
			}
			return path.Last().Item3;
		}

		[Part(0)]
		public object Part0(string input)
		{
			var state = new State(14)
			{
				HP = 10,
				Mana = 250,
			};

			Console.WriteLine(state);
			state = state.TickEffects().MakeRecharge();
			Console.WriteLine("MakeRecharge:");
			Console.WriteLine(state);
			state = state.TickEffects().BossAttack(8);
			Console.WriteLine("BossAttack:");
			Console.WriteLine(state);
			state = state.TickEffects().MakeShield();
			Console.WriteLine("MakeShield:");
			Console.WriteLine(state);
			state = state.TickEffects().BossAttack(8);
			Console.WriteLine("BossAttack:");
			Console.WriteLine(state);
			state = state.TickEffects().MakeDrain();
			Console.WriteLine("MakeDrain:");
			Console.WriteLine(state);
			state = state.TickEffects().BossAttack(8);
			Console.WriteLine("BossAttack:");
			Console.WriteLine(state);
			state = state.TickEffects().MakePoison();
			Console.WriteLine("MakePoison:");
			Console.WriteLine(state);
			state = state.TickEffects().BossAttack(8);
			Console.WriteLine("BossAttack:");
			Console.WriteLine(state);
			state = state.TickEffects().MakeMagicMissile();
			Console.WriteLine("MakeMagicMissile:");
			Console.WriteLine(state);
			state = state.TickEffects().BossAttack(8);
			Console.WriteLine("BossAttack:");
			Console.WriteLine(state);

			return null;
		}

		[Part(1)]
		public object Part1(string input)
		{
			var (bossHP, bossD) = ParseInput(input);
			var start = new State(bossHP);

			BFS.Search(start, s => s.StepTurns(bossD), s => s.BossHP <= 0, out var path);
			Console.WriteLine(start);
			foreach (var (action, step, cost) in path)
			{
				Console.WriteLine($"Make {action} for a total of {cost}");
				Console.WriteLine(step);
			}
			return path.Last().Item3;
		}

		[Part(2)]
		public object Part2(string input)
		{
			var (bossHP, bossD) = ParseInput(input);
			var start = new State(bossHP);

			BFS.Search(start, s => s.StepTurns(bossD, true), s => s.BossHP <= 0, out var path);
			Console.WriteLine(start);
			foreach (var (action, step, cost) in path)
			{
				Console.WriteLine($"Make {action} for a total of {cost}");
				Console.WriteLine(step);
			}
			return path.Last().Item3;
		}

		private (long hp, long d) ParseInput(string input)
		{
			var lines = input.Lines().Select(l => Int64.Parse(l.Split(": ")[1])).ToArray();
			return (lines[0], lines[1]);
		}

		private readonly struct State
		{
			private const long ShieldTicks = 6;
			private const long ShieldArmor = 7;
			private const long PoisonTicks = 6;
			private const long PoisonDamage = 3;
			private const long RechargeTicks = 5;
			private const long RechargeAmount = 101;

			private const long MagicMissileMana = 53;
			private const long DrainMana = 73;
			private const long ShieldMana = 113;
			private const long PoisonMana = 173;
			private const long RechargeMana = 229;

			public long HP { get; init; } = 50;
			public long Mana { get; init; } = 500;
			public long Shield { get; init; } = 0;
			public long Poison { get; init; } = 0;
			public long Recharge { get; init; } = 0;

			public long BossHP { get; init; }

			public State(long bossHP) =>
				(BossHP) = (bossHP);

			public IEnumerable<(string, State, long)> StepTurns(long bossDamage, bool hard = false) =>
				(this with { HP = hard ? HP - 1 : HP })
					.TickEffects()
					.MakeMoves()
					.Select(p => (p.Item1, p.Item2.TickEffects().BossAttack(bossDamage), p.Item3))
					.Where(p => p.Item2.HP > 0);

			private IEnumerable<(string, State, long)> MakeMoves()
			{
				if (Mana >= MagicMissileMana)
				{
					yield return ("MagicMissile", MakeMagicMissile(), MagicMissileMana);
				}
				if (Mana >= DrainMana)
				{
					yield return ("Drain", MakeDrain(), DrainMana);
				}
				if (Mana >= ShieldMana && Shield == 0)
				{
					yield return ("Shield", MakeShield(), ShieldMana);
				}
				if (Mana >= PoisonMana && Poison == 0)
				{
					yield return ("Poison", MakePoison(), PoisonMana);
				}
				if (Mana >= RechargeMana && Recharge == 0)
				{
					yield return ("Recharge", MakeRecharge(), RechargeMana);
				}
			}
			public State MakeMagicMissile() =>
				this with { Mana = Mana - MagicMissileMana, BossHP = BossHP - 4 };
			public State MakeDrain() =>
				this with { Mana = Mana - DrainMana, HP = HP + 2, BossHP = BossHP - 2 };
			public State MakeShield() =>
				this with { Mana = Mana - ShieldMana, Shield = Shield + ShieldTicks };
			public State MakePoison() =>
				this with { Mana = Mana - PoisonMana, Poison = Poison + PoisonTicks };
			public State MakeRecharge() =>
				this with { Mana = Mana - RechargeMana, Recharge = Recharge + RechargeTicks };

			public State TickEffects() =>
				TickShield()
					.TickPoison()
					.TickRecharge();
			private State TickShield() =>
				this with { Shield = Math.Max(0, Shield - 1) };
			private State TickPoison()
			{
				if (Poison == 0)
				{
					return this;
				}
				else
				{
					return this with
					{
						Poison = Poison - 1,
						BossHP = BossHP - PoisonDamage,
					};
				}
			}
			private State TickRecharge()
			{
				if (Recharge == 0)
				{
					return this;
				}
				else
				{
					var newTicks = Recharge - 1;
					return this with
					{
						Recharge = Recharge - 1,
						Mana = Mana + RechargeAmount,
					};
				}
			}

			public State BossAttack(long bossDamage) =>
				this with { HP = HP - (BossHP > 0 ? Math.Max(1, bossDamage - ShieldArmor * Math.Min(Shield, 1)) : 0) };

			public override bool Equals(object obj) =>
				obj is State s &&
					s.HP == HP &&
					s.Mana == Mana &&
					s.Shield == Shield &&
					s.Poison == Poison &&
					s.Recharge == Recharge &&
					s.BossHP == BossHP;

			public override int GetHashCode() =>
				HashCode.Combine(HP, Mana, Shield, Poison, Recharge, BossHP);

			public override string ToString()
			{
				var sb = new StringBuilder();
				sb.AppendLine($"Player has {HP} hit points, {Shield} armor, {Mana} mana");
				sb.AppendLine($"Boss has {BossHP} hit points");
				sb.AppendLine($"Shield is at {ShieldArmor}, its times is {Shield}");
				sb.AppendLine($"Poison is at {PoisonDamage}, its times is {Poison}");
				sb.AppendLine($"Recharge is at {RechargeAmount}, its times is {Recharge}");
				return sb.ToString();
			}
		}
	}
}
