# Treeleaves

Treeleaves is a program designed to create file tags based on directory location
of the file. Treeleaves is made with Common Lisp (SBCL).

## Usage

To run treeleaves run:
```
./treeleaves.lisp -d ~/Documents
```

This will generate a list of tags for your files
inside of an sqlite database `documents.sqlite`
in your current working directory.

## Installation

Run `make build` which should fetch all the required packages
and create the `treeleaves` binary inside `bin`. Treeleaves requires:

- uiop
- cl-utilities
- mito
- unix-opts

`asdf` is used under the hood to build the project, so it is also required to build
from source.
