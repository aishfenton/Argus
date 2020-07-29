# Contributing

Contributions encouraged! Please send a pull-request to our github repo.

# Running the tests
 
```bash
sbt test
 ```

# Releasing

Releases are managed by the core team, but documenting the process here 
because we sometimes forget too :)

The workflow for releases are managed through sbt. You need to have ```~/.sbt/0.13/sonatype.sbt``` 
configured with the Sonatype credentials (which core contributors should have)

1. Run the following and follow prompts for version bumps, etc. This script
runs the tests, bumps the version, tags the release, commits those changes,
releases the package to Sonatype, and completes the Sonatype release
process.

    ```bash
    sbt release
    ```

2. Go to the Argus github [repo](https://github.com/aishfenton/Argus) and
add release notes under "releases" 


