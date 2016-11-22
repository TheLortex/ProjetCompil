#!/bin/bash

eval `opam config env`
ocamlbuild test.byte -use-menhir -use-ocamlfind -pkg dot
