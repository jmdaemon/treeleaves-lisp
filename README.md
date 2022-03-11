# Treeleaves

> Automatically tag files on your system

Treeleaves is a program designed to create file tags based on directory location
of the file. Treeleaves is made with Common Lisp (SBCL).

## Why

Tagging all your files for you automatically by directory allows you to easily
search for them using any of these tags, and also preserve the structure of your
current file directories. Don't feel like using Treeleaves to search for your files
anymore? No problem, just delete the related `documents.sqlite` file, and you're done.

In the future Treeleaves will automatically tag new files and insert entries into your
document database. You'll be able to even specify other heuristics to tag files by,
and even provide automatic migrations should you move the files somewhere else on disk.

## Installation

Run `make build` which should fetch all the required packages
and create the `treeleaves` binary inside `bin`.

Then to install run `sudo make install` to install `treeleaves`.

### Requirements

Treeleaves requires the following common lisp packages:

- uiop
- cl-utilities
- mito
- unix-opts
- iterate

Additionally if you want to build the project from source you will need these dependencies.
- asdf
- rove

## Usage

To create the initial file database run:
``` bash
treeleaves -d ~/Documents -o ~/Documents/documents.sqlite -p "/**/*.pdf"
```

This will generate a `documents.sqlite` containing hierarchical tags
for all files specified in the directory matching the pattern glob in the output directory.

For argument defaults and more information run:
``` bash
treeleaves -h
```

To search for any matching file paths run:
``` bash
treeleaves -q "[:keyword]" "[search-term] %"
# Example:
# treeleaves -q ":tags" "Books %"
```
