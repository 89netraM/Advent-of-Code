CREATE TABLE Input(line TEXT, lineNumber SERIAL PRIMARY KEY);
COPY Input(line) FROM '/data/input.txt';

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

CREATE FUNCTION RunForklifts()
RETURNS INTEGER
LANGUAGE SQL
AS $$
    WITH ClearedPaperRolls AS (
        DELETE
        FROM Map
        WHERE (
            SELECT
                count(*)
            FROM
                Map Map2
            WHERE
                (Map2.x = Map.x OR Map2.x + 1 = Map.x OR Map2.x - 1 = Map.x)
                AND (Map2.y = Map.y OR Map2.y + 1 = Map.y OR Map2.y - 1 = Map.y)
                AND NOT (Map2.x = Map.x AND Map2.y = Map.y)
        ) < 4
        RETURNING *
    )
    SELECT
        CASE
            WHEN count(*) = 0 THEN 0
            ELSE count(*) + RunForklifts()
        END AS count
    FROM ClearedPaperRolls;
$$;

SELECT 'Part 2: ' || RunForklifts();
