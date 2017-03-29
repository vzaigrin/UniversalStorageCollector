# Universal Storage Collector

A tool for gathering performance information from different storage system.

This tool could extract performance information from:
- EMC VNX Block - using naviseccli;
- EMC VNX Block - from NAR files;
- EMC VNX File - by SSH to Control Station;
- EMC VMAX Block - from Unisphere reports;
- EMC VMAX File - from Control Station (use VNX File);
- EMC VPLEX - from VPLEX's monitors;
- EMC RecoverPoint - by REST API;
- IBM Storwize - by CLI.

This tool could output data to Carbon (Graphite) and InfluxDB.
It is possible to output to different Carbon and InfluxDB servers at same time.

## Compiling

USC written on Scala and should be compiled by SBT.
To add new Extractor and Output to USC you should add Classes for its and add reference to its to *Extractor* and *Output* classes.
Keep file *build.sbt* and *src* and *project* directory.
To compile use command *sbt compile*
To assembly fat jar use command *sbt assembly*
It's possible also to use IntelliJ IDEA.

## Deployment

To deploy USC:
- Create home directory for it (for example, */opt/USC*)
- Create environment variable USC_HOME referencing to this directory.
- Create subdirectory *bin*, *conf*, *log* and *pool* in that directory.
- Put *collector.jar* into *bin* subdirectory and *collector.xml* into *conf* subdirectory.

To start use command */usr/bin/java -DUSC_HOME=/opt/USC -cp /opt/USC/bin/collector.jar universalstoragecollector.collector* or create service description file.

## Configuration

USC uses one configuration file.
This file keeps configurations for:
- extractors - list of extractors, each extractor has it own common parameters
- outputs - list of outputs, each output has it own common parameters
- systems - list of storage systems, each system has it concrete parameters

Each storage system desciption should has:
- name - name of the storage system (for example, *array01*)
- class - name of storage system class (for example, *vnx*)
- type - #optional# name of storage system type for unified storage system (for eaxmple, *block*)
- interval - interval for periodic requests (for eaxmple, *10 min*)
- extractor - name and concrete parameters for the extractor using for this storage system
- output - name of the output instance

It's possible to use different extractors for same storage systems.
For example, we could use VNXBlock extractor to get configuration parameters and VNXNar to get performance data.

If you plan to use InfluxDB, you should create database with *dbname* specified in the configuration file *collector.xml*

## Internal details (how it works)

After parsing configuration file USC prepare list of storage system.

Each storage system has concrete extractor and output instance.

USC uses Actors System for parallel processing.

Each storage system is an actor.

Actors System send signal "ask" to each actor by its interval.

By receiving such signal each actor:
- send "start" signal to output;
- send "ask" signal to extractor;
- send "stop" signal to output.

By receiving "ask" signal each extractor:
- get "raw" data from storage system;
- proceed "raw" data;
- output proceding data to output.

Extractor send each piece of prepared data in two data structures:
- msg - Set of numbered pairs: number -> (parameter, value)
        For example: *1 -> ("server", "server_2"),  2 -> ("pool", "Pool_0")*
- data - Set of measurement pairs: (measurement, value)
         For example: *"iops" -> 100, "bw" -> 20, "rt" -> 5*

Carbon uses values from *msg* in ordered number to prepare header.
For example, *1 -> ("server", "server_2"),  2 -> ("pool", "Pool 0")* will be output as *server_2.Pool_0*
InfluxDB uses *msg* for creating *fields* in database.

## Extractors details

### EMC VNX Block

VNXBlock uses *naviseccli* to get data from SP.
*Methods* in common extractor parameters describe naviseccli command to use and list of values to extract.
There two type of such methods: *simple* and *flat*
*Simple* means that we get only list of values for asking instance. For eaxmple, values for SP.
*Flat* means that we get a list of instances followed by values. For example, Disk id and it values.
For each concrete storage system using VNXBlock extractor we specify methods (from global methods list) to use.
For example, there are method "cache" for VNX1 and method *cache2* for VNX2 in the global methods list.
We will specify concreate method "cache" for concreate storage system.

### EMC VNX File

VNXFile uses *server_stats* command on Control Station.
*Methods* in common extractor parameters describe server_stats command parameters to use and type of result.
Types could be:
- simple - We get only measurement name and it values
- composite - We get parameter name, measurement name and it value.
              For eaxmple, device NetworkingIn 10
- composite2 - We get two parameters name, measurement name and it value.
               For eaxmple, filesystem client OpName uSecCall 10
- composite3 - we get three parameters name, measurement name and it value. For eaxmple,

For each concrete storage system using VNXFile extractor we specify methods (from global methods list) to use.
For eaxmple, we specify methods for CIFS only for storage systems with CIFS


### EMC VNX Nar

VNX Nar extractor takes Nar file from storage system and parse it.
Periodical generating of Nar files should be enabled on the storage system.
VNXNar extractor keeps name of last proceeded Nar file.
When new Nar file arrived on the storage system, extractor takes it.
Extractor proceed only last new Nar file, if more than one files are available.
We specify names of measurements we need to extract from Nar file for concrete storage system.

### EMC VPLEX

VPLEX uses *monitors* created on concrete VPLEX system.
There are no global methods list for VPLEX extractor.
For concrete storage system we specify *director* name and *monitor* file name.
VPLEX extractor takes last record from each monitor file.

### EMC VMAX

VMAX uses *reports* running on the Unisphere server.
Periodical generating reports should be enabled.
There are no global methods list for VMAX extractor.
For concrete storage system we specify *SID* name and path to *report* files.

### EMC RecoverPoint

This is the less configurable extractor comparing to other extractors.
We specify only *baseUrl* for REST requests in common parameters.
For concreate storage system we specify version of the RecoverPoint software (for eaxmple, 4_4 or 5_0).
From each RecoverPoint extractor gets data about *clusters*, *RPAs* and *Groups*.

### IBM Storwize

This extractor uses CLI commands to get data from Storwize systems.
This commands described in common part of this extractor parameters.

---
Some details are available here: https://vzaigrin.wordpress.com/2017/01/28/universal-storage-collector
