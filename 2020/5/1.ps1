function Binary {
	param (
		[Char[]]
		$s,
		[Char]
		$low,
		[Char]
		$high,
		[Int]
		$max
	)
	$min = 0;
	$s | ForEach-Object {
		if ($_ -eq $low) {
			$max = [Int](($max - $min) / 2) + $min;
		}
		else {
			$min = [Int](($max - $min) / 2) + $min;
		}
	}
	return $min;
}

(Get-Content ".\input.txt" | ForEach-Object {
	return (Binary $_.ToCharArray()[0..7] 'F' 'B' 128) * 8 + (Binary $_.ToCharArray()[7..10] 'L' 'R' 8);
} | Measure-Object -Maximum).Maximum