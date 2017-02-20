# Universal Storage Collector

A tool for gathering performance information from different storage system.

This tool could extract performance information from:
- VNX Block using naviseccli;
- VNX Block from NAR files;
- VNX File from Control Station;
- VMAX Block from Unisphere reports;
- VMAX File from Control Station (similar to VNX File);
- VPLEX from VPLEX's monitors.

This tool could output data to Carbon (Graphite) and InfluxDB.
It is possible to output to different Carbon and InfluxDB servers.

You should specify storage systems you plan to extract data from.
For each storage system you should define *extractor* and *output*.
Each storage system output to its outputer.
For Unified storage system you should define separate descriptions for Block and File subsystems.

To deploy this tool you should create a directory for it and create subdirectory *conf* and *log*.
If you plan to extract data from NAR files (use *vnxnar* extractor) you should create subdirectory *pool*.

If you plan to use InfluxDB, you should create database with dbname specified in the configuration file *collector.xml*

To start this tool you should setup USC_HOME environment variable.

See details here: https://vzaigrin.wordpress.com/2017/01/28/universal-storage-collector
