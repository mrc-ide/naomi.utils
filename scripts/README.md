# Scripts

This dir contains naomi related scripts including:

* copy_adr_data.R - copies datasets from 1 ADR year into another, we have used this in 2021 and 2022 to create intial datasets for country teams
* remove_2023_datasets.R - Removes all 2023 datasets from ADR

## copy ADR datasets

To run this we need an API key, it can sometimes be useful for someone with more privileges to run the script such as Ian or someone from Fjelltopp to avoid issues with dataset access. For this there is a docker container to make running easier.

### Build image

From `scripts` dir
```
docker build -t mrcide/adr-copy:latest .
```

### Push image

```
docker push mrcide/adr-copy:latest
```

### Run script

Show help
```
docker run --rm mrcide/adr-copy:latest -h
```

Dry run, key is your ADR API key
```
docker run --rm mrcide/adr-copy:latest --dry-run --key=<key>
```

Run on dev
```
docker run --rm mrcide/adr-copy:latest --key=<key>
```

Run on prod
```
docker run --rm mrcide/adr-copy:latest --site=prod --key=<key>
```

## Remove ADR datasets

### Build image

From `scripts` dir
```
docker build -t mrcide/remove-datasets:latest -f remove.Dockerfile .
```

### Push image

```
docker push mrcide/remove-datasets:latest
```

### Run script

Show help
```
docker run --rm mrcide/remove-datasets:latest -h
```

Dry run, key is your ADR API key
```
docker run --rm mrcide/remove-datasets:latest --dry-run --key=<key>
```

Run on dev
```
docker run --rm mrcide/remove-datasets:latest --key=<key>
```

Run on prod
```
docker run --rm mrcide/remove-datasets:latest --site=prod --key=<key>
```
