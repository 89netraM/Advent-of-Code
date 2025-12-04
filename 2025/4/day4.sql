CREATE TABLE Input(line TEXT, lineNumber INTEGER PRIMARY KEY AUTOINCREMENT);
.import input.txt Input

CREATE TABLE Map(x INTEGER, y INTEGER, neighbors INTEGER, PRIMARY KEY (x, y));
WITH RECURSIVE Splitter AS (
   SELECT
       lineNumber as y,
       1 as x,
       substring(line, 1, 1) AS part,
       substring(line, 2) AS remainder
   FROM Input
   UNION ALL
   SELECT
       y,
       x + 1 as x,
       substring(remainder, 1, 1),
       substring(remainder, 2)
   FROM Splitter
   WHERE remainder != ''
)
INSERT INTO Map SELECT x, y, 0 AS neighbors FROM Splitter WHERE part = '@';

UPDATE
    Map
SET
    neighbors = (
        SELECT
            count(*)
        FROM
            Map Map2
        WHERE
            (Map2.x = Map.x OR Map2.x + 1 = Map.x OR Map2.x - 1 = Map.x)
            AND (Map2.y = Map.y OR Map2.y + 1 = Map.y OR Map2.y - 1 = Map.y)
            AND NOT (Map2.x = Map.x AND Map2.y = Map.y)
    );

SELECT 'Part 1: ' || (SELECT count(*) FROM Map WHERE neighbors < 4);
