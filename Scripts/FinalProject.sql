SELECT
    *
FROM
    movie;


---1. What are the average number of English speaking films released each year over the most recent 15 years?

SELECT
    round(AVG(COUNT(movieid)), 2) AS "Average Released Movies"
FROM
    movie
WHERE
    startyear > '2005';

---2. Categorize #1 by genre.

SELECT
    round(SUM(drama) / 15, 2) AS "Drama",
    round(SUM(romance) / 15, 2) AS "Romance",
    round(SUM(thriller) / 15, 2) AS "Thriller",
    round(SUM(comedy) / 15, 2) AS "Comedy",
    round(SUM(sci_fi) / 15, 2) AS "SCI-FI",
    round(SUM(adventure) / 15, 2) AS "Adventure",
    round(SUM(animation) / 15, 2) AS "Animation",
    round(SUM(children) / 15, 2) AS "Children",
    round(SUM(imax) / 15, 2) AS "IMAX",
    round(SUM(musical) / 15, 2) AS "Musical",
    round(SUM(action) / 15, 2) AS "Action",
    round(SUM(mystery) / 15, 2) AS "Mystery",
    round(SUM(crime) / 15, 2) AS "Crime",
    round(SUM(war) / 15, 2) AS "War",
    round(SUM(western) / 15, 2) AS "Western",
    round(SUM(fantasy) / 15, 2) AS "Fantasy",
    round(SUM(horror) / 15, 2) AS "Horror",
    round(SUM(documentary) / 15, 2) AS "Documentary",
    round(SUM(film_noir) / 15, 2) AS "Film-Noir",
    round(SUM(no_genre) / 15, 2) AS "No_Genre"
FROM
    genre
    JOIN movie ON genre.movieid = movie.movieid
WHERE
    startyear > '2005';

---3. What year had the largest number of films? By Genre? (Rank them?)

SELECT
    startyear   AS "Year",
    counting    AS "Number of Movies",
    RANK() OVER(
        PARTITION BY original_language
        ORDER BY
            counting DESC
    ) AS "Ranking"
FROM
    (
        SELECT
            startyear,
            original_language,
            COUNT(*) AS counting
        FROM
            movie
        GROUP BY
            startyear,
            original_language
    );
    
    
---8. Do Ratings directly correlate with Revenue?
--person's Correlation

SELECT
    ( round(((SUM(ml_rating * revenue) -(SUM(ml_rating) * SUM(revenue)) / COUNT(*))) /(sqrt(SUM(ml_rating * ml_rating) -(SUM(ml_rating
    ) * SUM(ml_rating)) / COUNT(*)) * sqrt(SUM(revenue * revenue) -(SUM(revenue) * SUM(revenue)) / COUNT(*))), 4) * 100 )
    || '%' AS "Pearsons r"
FROM
    movie
WHERE
    ml_rating != 0
    AND revenue != 0;

---13. Can you determine profitability of the movies?

SELECT
    movieid,
    title,
    round((revenue - budget) / revenue, 4) AS "Profit Margin"
FROM
    movie
WHERE
    revenue != 0
    AND budget != 0;

SELECT
    *
FROM
    movie;

SELECT DISTINCT
    ( actor1_name )
FROM
    movie
WHERE
    ml_rating > 4.5;