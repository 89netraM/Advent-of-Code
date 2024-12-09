using System.Collections.Generic;
using System.Linq;
using AoC.Library;

namespace AoC.Year2024;

[Day(9)]
public class Day9
{
    [Part(1)]
    public object Part1(string input)
    {
        var map = input.Select(c => (long)(c - '0')).ToArray();
        var files = new List<File> { new(0, 0, map[0]) };
        var offset = map[0];
        var id = 1L;
        for (int i = 1; i < map.Length; i += 2, id++)
        {
            offset += map[i];
            files.Add(new(id, offset, map[i + 1]));
            offset += map[i + 1];
        }

        var compactedFiles = new List<File>();
        offset = 0;
        while (files.Count > 0)
        {
            if (files.FirstOrDefault(f => f.Offset == offset) is File placedFile)
            {
                compactedFiles.Add(placedFile);
                offset += placedFile.Count;
                files.Remove(placedFile);
            }
            else if (files.FirstOrDefault(f => f.Offset > offset) is File nextPlacedFile)
            {
                var empty = nextPlacedFile.Offset - offset;
                var lastFile = files.MaxBy(f => f.Offset);
                if (lastFile.Count <= empty)
                {
                    compactedFiles.Add(lastFile with { Offset = offset });
                    offset += lastFile.Count;
                    files.Remove(lastFile);
                }
                else
                {
                    compactedFiles.Add(lastFile with { Offset = offset, Count = empty });
                    offset += empty;
                    files.Remove(lastFile);
                    files.Add(lastFile with { Count = lastFile.Count - empty });
                }
            }
            else
            {
                var lastFile = files.MaxBy(f => f.Offset);
                compactedFiles.Add(lastFile with { Offset = offset });
                offset += lastFile.Count;
                files.Remove(lastFile);
            }
        }
        return compactedFiles.Sum(f => f.Checksum);
    }

    [Part(2)]
    public object Part2(string input)
    {
        var map = input.Select(c => (long)(c - '0')).ToArray();
        var files = new List<File> { new(0, 0, map[0]) };
        var spaces = new List<Space>();
        var offset = map[0];
        var id = 1L;
        for (int i = 1; i < map.Length; i += 2, id++)
        {
            spaces.Add(new(offset, map[i]));
            offset += map[i];
            files.Add(new(id, offset, map[i + 1]));
            offset += map[i + 1];
        }

        for (id = files.Max(f => f.Id); id >= 0; id--)
        {
            var fileToMove = files.Find(f => f.Id == id)!;
            if (
                spaces.FirstOrDefault(s =>
                    s.Count >= fileToMove.Count && s.Offset < fileToMove.Offset
                )
                is Space targetSpace
            )
            {
                spaces.Remove(targetSpace);
                if (targetSpace.Count > fileToMove.Count)
                {
                    spaces.Add(
                        new(
                            targetSpace.Offset + fileToMove.Count,
                            targetSpace.Count - fileToMove.Count
                        )
                    );
                    spaces.Sort((a, b) => (int)(a.Offset - b.Offset));
                }
                files.Remove(fileToMove);
                files.Add(fileToMove with { Offset = targetSpace.Offset });
            }
        }

        return files.Sum(f => f.Checksum);
    }

    record File(long Id, long Offset, long Count)
    {
        public long Checksum => Enumerable.Range((int)Offset, (int)Count).Sum(o => o * Id);
    }

    record Space(long Offset, long Count);
}
