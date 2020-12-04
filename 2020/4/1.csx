using System.Text.RegularExpressions;

bool isValid(string[] inp)
{
	return inp.Any(f => f.StartsWith("byr")) &&
		inp.Any(f => f.StartsWith("iyr")) &&
		inp.Any(f => f.StartsWith("eyr")) &&
		inp.Any(f => f.StartsWith("hgt")) &&
		inp.Any(f => f.StartsWith("hcl")) &&
		inp.Any(f => f.StartsWith("ecl")) &&
		inp.Any(f => f.StartsWith("pid"));
}

Regex clean = new Regex(@"\n([^\n])");
var a = clean.Replace(File.ReadAllText("./input.txt"), " $1").Split('\n').Select(s => isValid(s.Split(' ')));
WriteLine(a.Count(b => b));