long Transform(long loopSize, long subject, long value = 1)
{
	for (long i = 0; i < loopSize; i++)
	{
		value *= subject;
		value %= 20201227;
	}
	return value;
}

var input = File.ReadAllLines("input.txt");
var cardPublic = Int64.Parse(input[0]);
var doorPublic = Int64.Parse(input[1]);

long cardLoopSize = 1;
for (long value = 1; value != cardPublic; cardLoopSize++)
{
	value *= 7;
	value %= 20201227;
}
cardLoopSize--;

WriteLine(
	Transform(cardLoopSize, doorPublic)
);