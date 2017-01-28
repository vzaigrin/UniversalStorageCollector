# Universal Storage Collector

A tool for gathering performance information from different storage system.

This tool could extract performance information from:
- VNX Block using naviseccli;
- VNX Block from NAR files;
- VNX File;
- VMAX from Unisphere reports;
- VPLEX.

This tool could output data to Carbon (Graphite) and InfluxDB.

To deploy this tool you should create a directory for it and create subdirectory *conf* and *log* in it.
And subdirectory *pool* if you plan to extract data from NAR files.

To start this tool you should setup USC_HOME environment variable.
