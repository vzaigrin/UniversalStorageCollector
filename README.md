# Universal Storage Collector

A tool for gathering performance information from different storage system.

This tool could extract performance information from:
- EMC VNX Block using naviseccli;
- EMC VNX Block from NAR files;
- EMC VNX File from Control Station;
- EMC VMAX Block from Unisphere reports;
- EMC VMAX File from Control Station (similar to VNX File);
- EMC VPLEX from VPLEX's monitors.

This tool could output data to Carbon (Graphite) and InfluxDB.
It is possible to output to different Carbon and InfluxDB servers at same time.

You should specify storage systems you plan to extract data from.
For each storage system you should define *extractor* and *output*.
Each storage system output to its outputer.
For Unified storage system you should define separate descriptions for Block and File subsystems.
It is possible to use multiple extactors for same storage system and type.

To deploy this tool you should create a directory for it and create subdirectory *conf* and *log*.
If you plan to extract data from NAR files (use *vnxnar* extractor) you should create subdirectory *pool*.

If you plan to use InfluxDB, you should create database with *dbname* specified in the configuration file *collector.xml*

Before start this tool you should setup USC_HOME environment variable.

See details here: https://vzaigrin.wordpress.com/2017/01/28/universal-storage-collector
