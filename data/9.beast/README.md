``` r
require(here)
require(magrittr)
require(Biostrings)

# Set the root directory to knit at the project main level
require(knitr)
opts_knit$set(root.dir = here::here())  # Indexed to repository .git file
```

Java 8 Setup for MacOS
----------------------

-   Used by:
    -   `beauti.jar`
    -   `beast.jar`

<details><summary><b>1) Uninstall Previous JDK</b></summary>
<p>
To install an older version of java, any newer JDK versions need to be removed.

-   <https://www.java.com/en/download/help/mac_uninstall_java.xml>

### Mount error

NOTE: `JavaUninstallTool.dmg` returning error: no mountable file systems

-   From:
    -   <https://deciphertools.com/blog/2017-10-02-no-mountable-file-systems/>

> macOS Sierra (10.12) and earlier is not able to mount the new Apple File System (APFS). So if you're on macOS Sierra (10.12) or earlier and you ran hdiutil and see references to Apple\_APFS or error 112, the issue is likely legitimate incompatibility, and this disk image won't open on this Mac without an update to the operating system.

-   Currently running El Captitan (Extent of MacBook hardware)

``` shell
jason$ sw_vers
ProductName:    Mac OS X
ProductVersion: 10.11.6
BuildVersion:   15G22010
```

-   Download Location

``` shell
$ ls ~/Downloads/JavaUninstallTool.dmg
```

### Troubleshooting

-   Manual mounting by `hdiutil attach`
    -   Including the `-verbose` argument returns detailed disk mounting information
        -   Reference for [hdiutil command](https://superuser.com/questions/19426/im-unable-to-mount-a-dmg-getting-a-no-mountable-filesystems-error)

``` shell
hdiutil attach -verbose ~/Downloads/JavaUninstallTool.dmg
```

**Returns error 112**

``` shell
DIHLDiskImageAttach() returned 112
hdiutil: attach failed - no mountable file systems
```

<details><summary><b>Verbose </b><tt>stdout</tt></summary>
<p>

``` shell
$ hdiutil attach -verbose ~/Downloads/JavaUninstallTool.dmg
Initializing…
DIBackingStoreInstantiatorProbe: interface  0, score      100, CBSDBackingStore
DIBackingStoreInstantiatorProbe: interface  1, score    -1000, CBundleBackingStore
DIBackingStoreInstantiatorProbe: interface  2, score    -1000, CRAMBackingStore
DIBackingStoreInstantiatorProbe: interface  3, score      100, CCarbonBackingStore
DIBackingStoreInstantiatorProbe: interface  4, score    -1000, CDevBackingStore
DIBackingStoreInstantiatorProbe: interface  5, score    -1000, CCURLBackingStore
DIBackingStoreInstantiatorProbe: interface  6, score    -1000, CVectoredBackingStore
DIBackingStoreInstantiatorProbe: interface  0, score      100, CBSDBackingStore
DIBackingStoreInstantiatorProbe: interface  1, score    -1000, CBundleBackingStore
DIBackingStoreInstantiatorProbe: interface  2, score    -1000, CRAMBackingStore
DIBackingStoreInstantiatorProbe: interface  3, score      100, CCarbonBackingStore
DIBackingStoreInstantiatorProbe: interface  4, score    -1000, CDevBackingStore
DIBackingStoreInstantiatorProbe: interface  5, score    -1000, CCURLBackingStore
DIBackingStoreInstantiatorProbe: interface  6, score    -1000, CVectoredBackingStore
DIFileEncodingInstantiatorProbe: interface  0, score    -1000, CMacBinaryEncoding
DIFileEncodingInstantiatorProbe: interface  1, score    -1000, CAppleSingleEncoding
DIFileEncodingInstantiatorProbe: interface  2, score    -1000, CEncryptedEncoding
DIFileEncodingInstantiatorProbe: interface  0, score      900, CUDIFEncoding
DIFileEncodingNewWithBackingStore: CUDIFEncoding
DIFileEncodingNewWithBackingStore: instantiator returned 0
DIFileEncodingInstantiatorProbe: interface  0, score    -1000, CSegmentedNDIFEncoding
DIFileEncodingInstantiatorProbe: interface  1, score    -1000, CSegmentedUDIFEncoding
DIFileEncodingInstantiatorProbe: interface  2, score    -1000, CSegmentedUDIFRawEncoding
DIDiskImageInstantiatorProbe: interface  0, score     1000, CUDIFDiskImage
DIDiskImageInstantiatorProbe: interface  1, score        0, CSparseBundleDiskImage
DIDiskImageInstantiatorProbe: interface  2, score        0, CSparseDiskImage
CRawDiskImage: data fork length 0x0000000000096BC6 (617414) not a multiple of 512.
DIDiskImageInstantiatorProbe: interface  3, score     -100, CRawDiskImage
DIDiskImageInstantiatorProbe: interface  4, score        0, CDARTDiskImage
DIDiskImageInstantiatorProbe: interface  5, score        0, CDiskCopy42DiskImage
DIDiskImageInstantiatorProbe: interface  6, score    -1000, CNDIFDiskImage
DIDiskImageInstantiatorProbe: interface  8, score     -100, CShadowedDiskImage
DIDiskImageInstantiatorProbe: interface  9, score    -1000, CCFPlugInDiskImage
DIDiskImageInstantiatorProbe: interface 10, score     -100, CWrappedDiskImage
DIDiskImageNewWithBackingStore: CUDIFDiskImage
DIDiskImageNewWithBackingStore: instantiator returned 0
Verifying…
Verification completed…
Error 0 (Undefined error: 0).
expected   CRC32 $54757AD0
Attaching…
DI_kextWaitQuiet: about to call IOServiceWaitQuiet...
DI_kextWaitQuiet: IOServiceWaitQuiet took 0.000007 seconds
2019-09-08 15:00:17.091 diskimages-helper[637:16272] DIHelperHDID serveImage: attaching drive
{
    autodiskmount = 1;
    "hdiagent-drive-identifier" = "4AE9F948-E95F-407C-A449-30278D73B0F6";
    "unmount-timeout" = 0;
}
2019-09-08 15:00:17.094 diskimages-helper[637:16272] DIHelperHDID serveImage: connecting to myDrive 0x4F0B
2019-09-08 15:00:17.096 diskimages-helper[637:16272] DIHelperHDID serveImage: register _readBuffer 0x10d97e000
2019-09-08 15:00:17.096 diskimages-helper[637:16272] DIHelperHDID serveImage: activating drive port 19723
2019-09-08 15:00:17.097 diskimages-helper[637:16272] DIHelperHDID serveImage: set cache enabled=TRUE returned SUCCESS.
2019-09-08 15:00:17.097 diskimages-helper[637:16272] DIHelperHDID serveImage: set on IO thread=TRUE returned SUCCESS.
2019-09-08 15:00:17.100 diskimages-helper[637:16272] -processKernelRequest: will sleep received
Volume check completed…
Mounting…
2019-09-08 15:00:17.166 diskimages-helper[637:16264] -remountReturningDictionary: detaching because no mountable filesystems.
DI_kextDriveDisconnect: could not disconnect from IOHDIXHDDrive object - 268435459
diskimages-helper: DI_kextDriveDisconnect returned 268435459 ((ipc/send) invalid destination port).
Attaching…
Error 112 (no mountable file systems).
Finishing…
2019-09-08 15:00:18.218 diskimages-helper[637:16258] *** -[NSMachPort handlePortMessage:]: dropping incoming DO message because the connection is invalid
DIHLDiskImageAttach() returned 112
hdiutil: attach failed - no mountable file systems
```

</p>
</details>

### Disk Utility First Aid

-   No evident problems returned
    -   See [Apple Disk Utility Guide](https://support.apple.com/guide/disk-utility/repair-a-disk-dskutl1040/mac) for OS-specific guidelines.

### Uninstall Java from Terminal

-   Instructions to [uninstall Java](https://www.java.com/en/download/help/mac_uninstall_java.xml) from the command line
    -   Requires either administrative credentials or `sudo` execution

**Java Files**

-   Current JVM

``` shell
$ java -version
java version "1.6.0_65"
Java(TM) SE Runtime Environment (build 1.6.0_65-b14-468)
Java HotSpot(TM) 64-Bit Server VM (build 20.65-b04-468, mixed mode)

$ ls /Library/Java/JavaVirtualMachines/
1.6.0.jdk   jdk-10.0.1.jdk
```

-   Plug-in installation

``` shell
$ ls /Library/Internet\ Plug-Ins/
Default Browser.plugin      JavaAppletPlugin.plugin
Disabled Plug-Ins       Quartz Composer.webplugin
```

-   Preference Pane

``` shell
$ ls /Library/PreferencePanes/    
JavaControlPanel.prefPane
```

-   Java Application files

``` shell
$ ls -lh ~/Library/Application\ Support/Java/
total 0
drwxr-xr-x  6 jason  staff   204B Nov 29  2018 Java 10.0.99.0.10
```

-   To *remove all files*, use the following commands:

``` shell
# sudo rm -fr /Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin
# sudo rm -fr /Library/PreferencePanes/JavaControlPanel.prefPane
# sudo rm -fr ~/Library/Application\ Support/Java
```

-   To use an older java version, remove the newer JDK:

``` shell
# sudo rm -rf /Library/Java/JavaVirtualMachines/jdk-10.0.1.jdk
```

</p>
</details>

<details><summary><b>2) Install JDK 8</b></summary>
<p>

### Java Downloads

-   [Maintained Version Downloads](https://www.oracle.com/java/technologies/javase-jsp-downloads.html)
    -   Java Development Kit [Version 8](https://www.oracle.com/java/technologies/jdk8-downloads.html)
        -   BEAST requires Java version 6 or 8
    -   Downloading a JRE will only install java as a plug in
        -   Install the comparable JDK

> Warning: Do not install the Java software offered on the <http://java.com> website. This will install java only as a plug in to your web browser. This version of Java will not be able to run BEAST. - Mac [BEAST Installation](https://beast.community/install_on_mac)

-   From instructions for [OS X JDK 8 Installation](https://docs.oracle.com/javase/8/docs/technotes/guides/install/mac_jdk.html)

### Java Specifications

-   JRE Version 8 Update 221
    -   build 1.8.0\_221-b11

``` shell
Jasons-MacBook:~ Admin$ java -version
java version "1.8.0_221"
Java(TM) SE Runtime Environment (build 1.8.0_221-b11)
Java HotSpot(TM) 64-Bit Server VM (build 25.221-b11, mixed mode)
```

-   Internet Plug-in

``` shell
Jasons-MacBook:~ Admin$ /Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java -version
java version "1.8.0_221"
Java(TM) SE Runtime Environment (build 1.8.0_221-b11)
Java HotSpot(TM) 64-Bit Server VM (build 25.221-b11, mixed mode)
```

-   Path to `java_home`

``` shell
Jasons-MacBook:~ Admin$ /usr/libexec/java_home -V
Matching Java Virtual Machines (3):
    1.8.0_221, x86_64:  "Java SE 8" /Library/Java/JavaVirtualMachines/jdk1.8.0_221.jdk/Contents/Home
    1.6.0_65-b14-468, x86_64:   "Java SE 6" /Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
    1.6.0_65-b14-468, i386: "Java SE 6" /Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home

/Library/Java/JavaVirtualMachines/jdk1.8.0_221.jdk/Contents/Home
```

</p>
</details>

<details><summary><b>3) BEAST Installation and Setup</b></summary>
<p>

-   Links
    -   Installation [Overview](https://beast.community/installing)
    -   Mac [Install](https://beast.community/install_on_mac)
    -   CLI [Install](https://beast.community/install_on_unix#homebrew-package-manager-for-mac-os-x)

### Path to BEAST directory

``` shell
$ ls /Applications/BEASTv1.10.4/ | more
README.txt
VERSION HISTORY.txt
bin
doc
examples
images
lib
native
```

-   Modify path to the program /bin directory

``` shell
# Edit contents of the `~/.bash_profile` file
vi ~/.bash_profile
```

``` shell
export JAVA_HOME="/usr/libexec/java_home -v 1.8"
export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/server
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Library/TeX/texbin:/Applications/BEASTv1.10.4/
```

-   Restart computer or reload changes to the profile

``` shell
source ~/.bash_profile
```

</p>
</details>

BEAST Workflow
==============

The `Biostrings` and `ape` packages were used to read in the subset alignments from the concatenated analysis and write `NEXUS` formatted alignment files.

``` r
# Iterate aligned subsets from concatenated analysis to write nexus files.
for (fasta in list.files("data/5.alignments-concatenated",
                         full.names = TRUE)) {
  locus_name <- strsplit(fasta, "/")[[1]][3] %>% gsub(".fasta", "", x = .)
  print(locus_name)
  locus_fasta <- Biostrings::readDNAStringSet(filepath = fasta)
  names(locus_fasta) <- gsub(" .+$", "", names(locus_fasta))
  char_wrap <- lapply(locus_fasta, length) %>% unique() %>% unlist()
  ape::write.nexus.data(x = locus_fasta, interleaved = FALSE,
                        charsperline = char_wrap,
                        file = paste0("data/9.beast/1.gene_nex_files/",
                                      locus_name, ".nex"))
}
```
