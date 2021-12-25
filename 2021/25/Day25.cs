// --- Day 25: Sea Cucumber ---
// This is it: the bottom of the ocean trench, the last place the sleigh keys could be. Your submarine's experimental antenna still isn't boosted enough to detect the keys, but they must be here. All you need to do is reach the seafloor and find them.
// At least, you'd touch down on the seafloor if you could; unfortunately, it's completely covered by two large herds of sea cucumbers, and there isn't an open space large enough for your submarine.
// You suspect that the Elves must have done this before, because just then you discover the phone number of a deep-sea marine biologist on a handwritten note taped to the wall of the submarine's cockpit.
// "Sea cucumbers? Yeah, they're probably hunting for food. But don't worry, they're predictable critters: they move in perfectly straight lines, only moving forward when there's space to do so. They're actually quite polite!"
// You explain that you'd like to predict when you could land your submarine.
// "Oh that's easy, they'll eventually pile up and leave enough space for-- wait, did you say submarine? And the only place with that many sea cucumbers would be at the very bottom of the Mariana--" You hang up the phone.
// There are two herds of sea cucumbers sharing the same region; one always moves east (>), while the other always moves south (v). Each location can contain at most one sea cucumber; the remaining locations are empty (.). The submarine helpfully generates a map of the situation (your puzzle input). For example:
// v...>>.vv>
// .vv>>.vv..
// >>.>v>...v
// >>v>>.>.v.
// v>v.vv.v..
// >.>>..v...
// .vv..>.>v.
// v.v..>>v.v
// ....v..v.>
// Every step, the sea cucumbers in the east-facing herd attempt to move forward one location, then the sea cucumbers in the south-facing herd attempt to move forward one location. When a herd moves forward, every sea cucumber in the herd first simultaneously considers whether there is a sea cucumber in the adjacent location it's facing (even another sea cucumber facing the same direction), and then every sea cucumber facing an empty location simultaneously moves into that location.
// So, in a situation like this:
// ...>>>>>...After one step, only the rightmost sea cucumber would have moved:
// ...>>>>.>..After the next step, two sea cucumbers move:
// ...>>>.>.>.During a single step, the east-facing herd moves first, then the south-facing herd moves. So, given this situation:
// ..........
// .>v....v..
// .......>..
// ..........
// After a single step, of the sea cucumbers on the left, only the south-facing sea cucumber has moved (as it wasn't out of the way in time for the east-facing cucumber on the left to move), but both sea cucumbers on the right have moved (as the east-facing sea cucumber moved out of the way of the south-facing sea cucumber):
// ..........
// .>........
// ..v....v>.
// ..........
// Due to strong water currents in the area, sea cucumbers that move off the right edge of the map appear on the left edge, and sea cucumbers that move off the bottom edge of the map appear on the top edge. Sea cucumbers always check whether their destination location is empty before moving, even if that destination is on the opposite side of the map:
// Initial state:
// ...>...
// .......
// ......>
// v.....>
// ......>
// .......
// ..vvv..
// 
// After 1 step:
// ..vv>..
// .......
// >......
// v.....>
// >......
// .......
// ....v..
// 
// After 2 steps:
// ....v>.
// ..vv...
// .>.....
// ......>
// v>.....
// .......
// .......
// 
// After 3 steps:
// ......>
// ..v.v..
// ..>v...
// >......
// ..>....
// v......
// .......
// 
// After 4 steps:
// >......
// ..v....
// ..>.v..
// .>.v...
// ...>...
// .......
// v......
// To find a safe place to land your submarine, the sea cucumbers need to stop moving. Again consider the first example:
// Initial state:
// v...>>.vv>
// .vv>>.vv..
// >>.>v>...v
// >>v>>.>.v.
// v>v.vv.v..
// >.>>..v...
// .vv..>.>v.
// v.v..>>v.v
// ....v..v.>
// 
// After 1 step:
// ....>.>v.>
// v.v>.>v.v.
// >v>>..>v..
// >>v>v>.>.v
// .>v.v...v.
// v>>.>vvv..
// ..v...>>..
// vv...>>vv.
// >.v.v..v.v
// 
// After 2 steps:
// >.v.v>>..v
// v.v.>>vv..
// >v>.>.>.v.
// >>v>v.>v>.
// .>..v....v
// .>v>>.v.v.
// v....v>v>.
// .vv..>>v..
// v>.....vv.
// 
// After 3 steps:
// v>v.v>.>v.
// v...>>.v.v
// >vv>.>v>..
// >>v>v.>.v>
// ..>....v..
// .>.>v>v..v
// ..v..v>vv>
// v.v..>>v..
// .v>....v..
// 
// After 4 steps:
// v>..v.>>..
// v.v.>.>.v.
// >vv.>>.v>v
// >>.>..v>.>
// ..v>v...v.
// ..>>.>vv..
// >.v.vv>v.v
// .....>>vv.
// vvv>...v..
// 
// After 5 steps:
// vv>...>v>.
// v.v.v>.>v.
// >.v.>.>.>v
// >v>.>..v>>
// ..v>v.v...
// ..>.>>vvv.
// .>...v>v..
// ..v.v>>v.v
// v.v.>...v.
// 
// ...
// 
// After 10 steps:
// ..>..>>vv.
// v.....>>.v
// ..v.v>>>v>
// v>.>v.>>>.
// ..v>v.vv.v
// .v.>>>.v..
// v.v..>v>..
// ..v...>v.>
// .vv..v>vv.
// 
// ...
// 
// After 20 steps:
// v>.....>>.
// >vv>.....v
// .>v>v.vv>>
// v>>>v.>v.>
// ....vv>v..
// .v.>>>vvv.
// ..v..>>vv.
// v.v...>>.v
// ..v.....v>
// 
// ...
// 
// After 30 steps:
// .vv.v..>>>
// v>...v...>
// >.v>.>vv.>
// >v>.>.>v.>
// .>..v.vv..
// ..v>..>>v.
// ....v>..>v
// v.v...>vv>
// v.v...>vvv
// 
// ...
// 
// After 40 steps:
// >>v>v..v..
// ..>>v..vv.
// ..>>>v.>.v
// ..>>>>vvv>
// v.....>...
// v.v...>v>>
// >vv.....v>
// .>v...v.>v
// vvv.v..v.>
// 
// ...
// 
// After 50 steps:
// ..>>v>vv.v
// ..v.>>vv..
// v.>>v>>v..
// ..>>>>>vv.
// vvv....>vv
// ..v....>>>
// v>.......>
// .vv>....v>
// .>v.vv.v..
// 
// ...
// 
// After 55 steps:
// ..>>v>vv..
// ..v.>>vv..
// ..>>v>>vv.
// ..>>>>>vv.
// v......>vv
// v>v....>>v
// vvv...>..>
// >vv.....>.
// .>v.vv.v..
// 
// After 56 steps:
// ..>>v>vv..
// ..v.>>vv..
// ..>>v>>vv.
// ..>>>>>vv.
// v......>vv
// v>v....>>v
// vvv....>.>
// >vv......>
// .>v.vv.v..
// 
// After 57 steps:
// ..>>v>vv..
// ..v.>>vv..
// ..>>v>>vv.
// ..>>>>>vv.
// v......>vv
// v>v....>>v
// vvv.....>>
// >vv......>
// .>v.vv.v..
// 
// After 58 steps:
// ..>>v>vv..
// ..v.>>vv..
// ..>>v>>vv.
// ..>>>>>vv.
// v......>vv
// v>v....>>v
// vvv.....>>
// >vv......>
// .>v.vv.v..
// In this example, the sea cucumbers stop moving after 58 steps.
// Find somewhere safe to land your submarine. What is the first step on which no sea cucumbers move?
// To begin, get your puzzle input.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using AoC.Library;
using static AoC.Library.Functional;
using RegExtract;

namespace AoC.Year2021
{
	[Day(25)]
	public class Day25
	{
		[Part(1)]
		public object Part1(string input)
		{
			var map = input.Lines().SelectMany((l, y) => l.Select((c, x) => (v: new Vector2(x, y), c))).Where(p => p.c != '.')
				.ToDictionary(p => p.v, p => p.c == '>');
			var map2 = new Dictionary<Vector2, bool>();

			var max = map.Keys.Aggregate((a, b) => a.MaxParts(b)) + new Vector2(1, 1);
			
			// Console.WriteLine("Initial state:");
			// Print(map, max);

			long steps = 0L;
			for (bool cont = true; cont; steps++)
			{
				cont = false;
				foreach (var kvp in map.Where(kvp => kvp.Value))
				{
					var next = new Vector2(MathM.Mod(kvp.Key.X + 1, max.X), kvp.Key.Y);
					if (map.ContainsKey(next))
					{
						map2[kvp.Key] = true;
					}
					else
					{
						map2[next] = true;
						cont |= true;
					}
				}

				foreach (var kvp in map.Where(kvp => !kvp.Value))
				{
					var next = new Vector2(kvp.Key.X, MathM.Mod(kvp.Key.Y + 1, max.Y));
					if (map.TryGetValue(next, out bool b) && !b || map2.ContainsKey(next))
					{
						map2[kvp.Key] = false;
					}
					else
					{
						map2[next] = false;
						cont |= true;
					}
				}

				(map, map2) = (map2, map);
				map2.Clear();
				
				// Console.WriteLine($"After {steps + 1} steps:");
				// Print(map, max);
			}
			return steps;
		}

		static void Print(IReadOnlyDictionary<Vector2, bool> map, Vector2 max)
		{
			for (long y = 0; y < max.Y; y++)
			{
				for (long x = 0; x < max.X; x++)
				{
					Console.Write(map.TryGetValue(new Vector2(x, y), out bool b) ? b ? '>' : 'v' : '.');
				}
				Console.WriteLine();
			}
			Console.WriteLine();
		}
	}
}
