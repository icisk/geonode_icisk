- clone this repository
- get github token
- save it as GH_TOKEN in root directory
- change PATH_TO_MOCKUPS path in .env file
- add spain data (gpkg files 1999 to 2022) to csmockups/LL1_Spain/RShinyApp/data (you can find it on the fileserver)
- execute the following docker commands:

```
docker compose build --no-cache
docker compose up -d
```

- Navigate to http://localhost

What do after new push to RShhiny app:

```
docker compose stop
docker volume rm geonode-rshiny
docker compose build rshiny --no-cache
docker compose up -d
```
