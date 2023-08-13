- clone this repository
- get github token
- save it as GH_TOKEN in root directory
- add spain data (gpkg files 2010 to 2022) to /scripts/docker/Rshiny/data (...data needs to be created)
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
