CREATE TABLE Input(line TEXT, lineNumber INTEGER PRIMARY KEY AUTOINCREMENT);
.import input.txt Input

CREATE TABLE Map(x INTEGER, y INTEGER, PRIMARY KEY (x, y));
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
INSERT INTO Map SELECT x, y FROM Splitter WHERE part = '@';

SELECT 'Part 1: ' || (SELECT count(*) FROM (SELECT (
        SELECT
            count(*)
        FROM
            Map Map2
        WHERE
            (Map2.x = Map.x OR Map2.x + 1 = Map.x OR Map2.x - 1 = Map.x)
            AND (Map2.y = Map.y OR Map2.y + 1 = Map.y OR Map2.y - 1 = Map.y)
            AND NOT (Map2.x = Map.x AND Map2.y = Map.y)
    ) AS neighbors FROM Map) WHERE neighbors < 4);

WITH Iterations AS (
    WITH RECURSIVE Forklift AS (
        SELECT
            1 as iteration,
            (
                SELECT json_group_array(json_object('x', Map.x, 'y', Map.y))
                FROM Map
                WHERE
                (
                    SELECT count(*)
                    FROM Map Map2
                    WHERE
                        (Map2.x = Map.x OR Map2.x + 1 = Map.x OR Map2.x - 1 = Map.x)
                        AND (Map2.y = Map.y OR Map2.y + 1 = Map.y OR Map2.y - 1 = Map.y)
                        AND NOT (Map2.x = Map.x AND Map2.y = Map.y)
                ) >= 4
            ) AS paperRolls
        UNION ALL
        SELECT
            iteration + 1 AS iteration,
            (
                WITH ExistingPaperRolls AS (
                    SELECT
                        json_extract(value, '$.x') AS x,
                        json_extract(value, '$.y') AS y
                    FROM json_each(paperRolls)
                )
                SELECT json_group_array(json_object('x', p.x, 'y', p.y))
                FROM ExistingPaperRolls p
                WHERE
                (
                    SELECT count(*)
                    FROM ExistingPaperRolls p2
                    WHERE
                        (p2.x = p.x OR p2.x + 1 = p.x OR p2.x - 1 = p.x)
                        AND (p2.y = p.y OR p2.y + 1 = p.y OR p2.y - 1 = p.y)
                        AND NOT (p2.x = p.x AND p2.y = p.y)
                ) >= 4
            ) AS nextPaperRolls
        FROM Forklift
        WHERE nextPaperRolls != paperRolls
    )
    SELECT max(iteration), json_array_length(paperRolls, '$') AS remainingPaperRolls FROM Forklift
)
SELECT 'Part 2: ' || (SELECT (SELECT count(*) FROM Map) - (SELECT remainingPaperRolls FROM Iterations));
