using System.Text.RegularExpressions;

Regex regex = new Regex(@"^(\d+)-(\d+) (.): (.+)$");

int IsValid(string line)
{
	Match match = regex.Match(line);
	int low = Int32.Parse(match.Groups[1].Value);
	int high = Int32.Parse(match.Groups[2].Value);
	char c = match.Groups[3].Value[0];
	string password = match.Groups[4].Value;
	int actual = password.Sum(x => x == c ? 1 : 0);
	return low <= actual && actual <= high ? 1 : 0;
}

WriteLine(File.ReadAllLines("./input.txt").Sum(IsValid));