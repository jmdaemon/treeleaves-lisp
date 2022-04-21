# Treeleaves

> Automatically tag files on your system

Treeleaves is a program designed to create file tags based on directory location
of the file. Treeleaves is made with Common Lisp (SBCL).

## Why

Tagging files on your system automatically allows you to easily
search for them using any of these tags while requiring little effort to tag them manually.
You only have to sort files into their respective folders and allow treeleaves to take
advantage of the hierarchical nature of the files. You can maintain your current directory
structure while also having the ability to quickly locate files in nested directories.
Don't feel like using Treeleaves to search for your files anymore? No problem,
just delete the related `documents.sqlite` file, and that's it.

Treeleaves also offers additional features such as:

- Automatic pruning of files not found on disk
- Automatic migration of file paths for files moved on disk
- Ability to specify specific files/directories to prune
- Ability to output generated directory tags

Other file searching/tagging heuristics are also planned for the future.

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
