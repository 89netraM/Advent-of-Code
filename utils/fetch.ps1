[CmdletBinding(DefaultParameterSetName = "NoDelay")]
param (
	[Parameter(Position = 0)]
	[int]
	$Year,
	[Parameter(Position = 1)]
	[int]
	$Day,
	[Parameter(Position = 2)]
	[string]
	$Language = "cs",
	[Parameter(ParameterSetName = "Time")]
	[ValidatePattern("\d{1,2}:\d{2}(:\d{2})?")]
	[string]
	$Time,
	[Parameter(ParameterSetName = "Delay")]
	[switch]
	$Delay
)

if (!$PSBoundParameters.ContainsKey("Year")) {
	$Year = Get-Date -Format "yyyy";
}
if (!$PSBoundParameters.ContainsKey("Day")) {
	$Day = Get-Date -Format "dd";
}

$basePath = Resolve-Path "$(Split-Path -Parent $MyInvocation.MyCommand.Path)\..\";
$yearPath = "$basePath\$Year";
if (!(Test-Path $yearPath)) {
	New-Item -ItemType Directory -Force -Path $yearPath;
}
$dayPath = "$yearPath\$Day";
if (!(Test-Path $dayPath)) {
	New-Item -ItemType Directory -Force -Path $dayPath;
}

if ($Delay.IsPresent) {
	$Time = "06:00:01";
}

if ($PSBoundParameters.ContainsKey("Time") -or $Delay.IsPresent) {
	$timeSpan = New-TimeSpan -End $Time;
	if ($timeSpan.TotalSeconds -gt 0.0) {
		Write-Output "Will wait $($timeSpan.TotalSeconds) seconds until $Time";
		Start-Sleep -Seconds $timeSpan.TotalSeconds;
	}
}

try {
	$headers = @{ "cookie" = (Get-Content "$basepath\utils\aoccookie") };

	try {
		(Invoke-WebRequest -Headers $headers -Uri "https://adventofcode.com/$Year/day/$Day/input").Content | Out-File -Path "$dayPath\input.txt" -NoNewline;

		$sourceFile = "$dayPath\Day$Day.$Language";

		$descHead = 15;
		$descFoot = 4;

		$html = New-Object -ComObject "HTMLFile";
		$result = Invoke-WebRequest -Headers $headers -Uri "https://adventofcode.com/$Year/day/$Day";
		$html.write([System.Text.Encoding]::Unicode.GetBytes($result.Content));
		$description = $html.body.innerText -split "`r`n";
		$description[$descHead..($description.Length - $descFoot)] | ForEach-Object { "// $_" } | Out-File -Path $sourceFile;

		$locationInFile = $description.Length - ($descHead + $descFoot) + 1;

		if (Test-Path "$basepath\utils\templates\template.$Language") {
			$template = Get-Content "$basepath\utils\templates\template.$Language" -Raw;
			
			$inserts = $template -split "`r`n" | Select-String '\$insert';
			if ($inserts.Length -gt 0) {
				$locationInFile += $inserts[0].LineNumber;
				$locationInFile = "$($locationInFile):$($inserts[0].Matches.Index + 1)";
			}

			$template -ireplace '\$year', "$Year" -ireplace '\$day', "$Day" -ireplace '\$insert', "" | Out-File -Path $sourceFile -Append -NoNewline;
		}

		Set-Location -Path $dayPath;

		code --goto "$($sourceFile):$locationInFile";
	}
	catch {
		Write-Error "Failed to fetch the description and input for $Year/$Day";
	}
}
catch {
	Write-Error "Place your cookies in utils\aoccookie";
}
