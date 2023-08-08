- get github token
- save it as GH_TOKEN in root directory
- add spain data (gpkg files 2010 to 2022) to /scripts/docker/Rshiny/data (...data needs to be created)

```
docker compose build --no-cache
docker compose up -d
```

go http://localhost

what do after new push to rshiny app
stop all dockers

```
docker volume rm geonode-rshiny
docker compose build rshiny --no-cache
docker compose up -d
```
