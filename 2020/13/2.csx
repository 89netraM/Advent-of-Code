using System.Runtime.InteropServices;
using System.Web;

var lines = File.ReadAllText("input.txt").Split("\n");

string query = String.Join(
	", ",
	lines[1]
		.Split(",")
		.Select((x, i) => (i: (long)i, x))
		.Where(x => x.x != "x")
		.Select(x => $"(t + {x.i}) mod {x.x} = 0")
);
string url = $"https://www.wolframalpha.com/input/?i={HttpUtility.UrlEncode(query)}";

if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
{
	Process.Start(new ProcessStartInfo("cmd", $"/c start {url.Replace("&", "^&")}") { CreateNoWindow = true });
}
else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
{
	Process.Start("xdg-open", url);
}
else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
{
	Process.Start("open", url);
}