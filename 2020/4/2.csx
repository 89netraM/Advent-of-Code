using System.Text.RegularExpressions;

Regex byr = new Regex(@"byr:(\d{4})");
Regex iyr = new Regex(@"iyr:(\d{4})");
Regex eyr = new Regex(@"eyr:(\d{4})");
bool year(string s, Regex r, int l, int h)
{
	var m = r.Match(s);
	if (m.Success) {
		int y = Int32.Parse(m.Groups[1].Value);
		return l <= y && y <= h;
	}
	return false;
}
Regex hgtR = new Regex(@"hgt:(\d+)(cm|in)");
bool hgt(string s)
{
	var m = hgtR.Match(s);
	if (m.Success)
	{
		int h = Int32.Parse(m.Groups[1].Value);
		if (m.Groups[2].Value == "cm")
		{
			return 150 <= h && h <= 193;
		}
		else
		{
			return 59 <= h && h <= 76;
		}
	}
	return false;
}
Regex hcl = new Regex(@"hcl:#[a-f0-9]{6}");
Regex ecl = new Regex(@"ecl:(amb|blu|brn|gry|grn|hzl|oth)");
Regex pid = new Regex(@"pid:\d{9}\t*$");

bool isValid(string[] inp)
{
	return inp.Any(f => year(f, byr, 1920, 2002)) &&
		inp.Any(f => year(f, iyr, 2010, 2020)) &&
		inp.Any(f => year(f, eyr, 2020, 2030)) &&
		inp.Any(hgt) &&
		inp.Any(hcl.IsMatch) &&
		inp.Any(ecl.IsMatch) &&
		inp.Any(pid.IsMatch);
}

Regex clean = new Regex(@"\n([^\n])");
var a = clean.Replace(File.ReadAllText("./input.txt"), " $1").Split('\n').Select(s => isValid(s.Split(' ')));
WriteLine(a.Count(b => b));