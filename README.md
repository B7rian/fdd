
Piggyback is in early development and doesn't do anything just yet.  This 
document describes what I'm currently thinking for this project.  Your
comments are welcome.

# Introduction

Piggyback is a command-line backup and archival program with the 
following features:

- Uses an easy to understand repository format that can be examined with 
  standard file managers, commandline tool, text editors, and data 
  integrity tools
- Allows the repository to be checked for integrity at any time using 
  commonly available 3rd party tools
- Supports recovery from data corruption or loss at file, repository, 
  partition, or device level
- User can control tradeoff between backup size and ability to recover 
  from errors
- Supports restoration of files using existing filesystem navigation methods

The idea here is that you can back up your files up to multiple cheap and unreliable 
sources (think USB drives, SD cards, phones) and not worry about losing your data. 
Piggyback supports this by keeping redundant copies of backed-up 
files within the repository and in repository mirrors. It reduces disk usage 
within a repository or mirror by linking duplicate files together (with 
symlinks if the target filesystem supports it, or via metadata if not) 
once redundancy targets are met; multiple copies of the same data "piggyback" 
off of one another.

# Usage

## Basic commands

Create a snapshot.  Directories are archived recursively.
> piggyback * ~/backups

Check an existing repository for problems
> piggyback -c ~/backups 

Check an existing repository for problems and repair issues
> piggyback -f ~/backups 

## Repository mirrors

# Terminology
