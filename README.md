
fdd is in early development and doesn't do much just yet.

# Introduction

fdd is a command-line backup and archival program with the 
following features:
- Uses an easy to understand repository format that can be examined with 
  standard file managers, commandline tools, text editors, and data 
  integrity tools (sha256sum, for example)
- Allows the repository to be checked for integrity at any time using 
  commonly available 3rd party tools (sha256sum, for example)
- Supports recovery from data corruption or loss:  
  individual files, repositories, partitions, or whole devices
- User can control tradeoff between backup size and ability to recover 
  from errors e.g. number of redundant copies
- Supports restoration of files using existing filesystem navigation
  methods.  The repository is just another directory, so find the file
  you want and copy it to where you need it.
- Can restart partially completed backups including partial copies of
  large files
- Can use/add new storage devices without having to update old ones
- Can convert existing file-copy backups (made with rsync, cp, file
  managers) to fdd repo without re-copying everything

The idea here is that you can back up your files up to multiple cheap
and unreliable sources (think USB drives, SD cards, phones) and not
worry about losing your data.  fdd supports this by keeping
redundant copies of backed-up files within the repository and in
repository mirrors. It reduces disk usage within a repository or
mirror by linking duplicate files together (with symlinks if the
target filesystem supports it, or via metadata if not) once redundancy
targets are met. 

# Usage

## Basic commands

Create a snapshot.  Directories are archived recursively.
> fdd * ~/backups

Check an existing repository for problems
> fdd -c ~/backups 
> sha512sum -c ~/backups/checksums

Check an existing repository for problems and repair issues
> fdd -f ~/backups 

## HOWTO for some use cases

### I have this pile of rsynced files and I want to convert it to a piggyback repo.

### I have a bunch of files and a checksum file and want to convert it to a piggyback repo

### Same as above, but some of the files are corrupted or missing and checksum fails

# Terminology
