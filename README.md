- clone this repository
- get github token
- save it as GH_TOKEN in root directory
- change PATH_TO_MOCKUPS path in .env file to the path where you cloned icisk/csmockups
- add spain data (gpkg files 1999 to 2022) to your geoserver once its running (you can find it on the fileserver) using the method described [here](https://github.com/icisk/csmockups/tree/main/tools#how-to-use)
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
