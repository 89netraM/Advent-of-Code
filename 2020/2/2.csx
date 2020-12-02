using System.Text.RegularExpressions;

Regex regex = new Regex(@"^(\d+)-(\d+) (.): (.+)$");

int IsValid(string line)
{
	Match match = regex.Match(line);
	int pos1 = Int32.Parse(match.Groups[1].Value) - 1;
	int pos2 = Int32.Parse(match.Groups[2].Value) - 1;
	char c = match.Groups[3].Value[0];
	string password = match.Groups[4].Value;
	return (password[pos1] != password[pos2] && (password[pos1] == c || password[pos2] == c)) ? 1 : 0;
}

WriteLine(File.ReadAllLines("./input.txt").Sum(IsValid));