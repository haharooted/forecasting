### SQL packs:
#install.packages("DBI")
#install.packages("RMySQL")

library(DBI)
library(RMySQL)


host <- "94.130.119.210"
dbname <- "cvr_ingest"
user <- "cvr_ingest_user"
password <- "**********"

#connect to db
con <- dbConnect(RMySQL::MySQL(), 
                 host = host, 
                 dbname = dbname, 
                 user = user, 
                 password = password)

# fetch all based on specific industry
query <- "WITH combined_employment AS (
    SELECT * FROM Maanedsbeskaeftigelse
    UNION ALL
    SELECT * FROM erstMaanedsbeskaeftigelse
),
filtered_companies AS (
    SELECT enhedsnummer FROM virk_hovedbranche
    WHERE brancheid = 68
),
employment_data AS (
    SELECT 
        c.aar,
        c.maaned,
        c.aarsvaerk,
        c.ansatte
    FROM filtered_companies f
    JOIN combined_employment c ON f.enhedsnummer = c.enhedsnummer
)
SELECT 
    e.aar,
    e.maaned,
    SUM(e.aarsvaerk) as total_aarsvaerk,
    SUM(e.ansatte) as total_ansatte
FROM employment_data e
GROUP BY e.aar, e.maaned
ORDER BY e.aar, e.maaned;
"
result <- dbGetQuery(con, query)
print(result)
dbDisconnect(con)
write.csv(result, file = "total_employed_real_estate.csv")
