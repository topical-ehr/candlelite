#! /bin/bash

rm -f .config/dotnet-tools.json
dotnet new tool-manifest
dotnet tool install fable --local --version 4.19.3
