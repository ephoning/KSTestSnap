# KS Test with Snap (REST) front-end

An implementation in Haskell of the **2 parameter Komogorov-Smirnov test**. It reads in 2 JSON files with samples, computes and displays their CDFs, max distance, and makes a judgement on whether the null hypothesis (i.e., the distributions are not significantly different) is to be rejected.

The REST front-end is very basic: usage is as follows:

```
<host>:8000/kstest/<samples 1 file-name>/<samples 2 file-name>
```

The usage information can also be obtained via:

```
<host>:8000/
```

## TODO

- Note that currently, no explicit configuration of the Snap HTTP server is done. This results in the server binding on all of the host's IP addresses with a port value of 8000

- Note that this should really be refactored to allow for the use of request parameters to specify the sample input files with their full paths. Currently, the sample input files can only be specified without a path. The actual path to these file is composed with a statically defined 'base path' in the source code (see: *basePath* in file *src/app/Main.hs*
 
- Additionally, consider outputing the result as JSON.

- Better error handling
